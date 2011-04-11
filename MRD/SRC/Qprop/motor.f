C***********************************************************************
C    Module:  motor.f
C 
C    Copyright (C) 2005 Mark Drela 
C 
C    This program is free software; you can redistribute it and/or modify
C    it under the terms of the GNU General Public License as published by
C    the Free Software Foundation; either version 2 of the License, or
C    (at your option) any later version.
C
C    This program is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU General Public License for more details.
C
C    You should have received a copy of the GNU General Public License
C    along with this program; if not, write to the Free Software
C    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
C***********************************************************************

      SUBROUTINE MOTORQ(  OMEGA,   VOLT, IMOTYPE, PARMOT,NMPAR, 
     &               Q, Q_OMEGA, Q_VOLT,
     &               I, I_OMEGA, I_VOLT )
C-----------------------------------------------------------
C     Motor+gearbox   torque(rpm,Voltage)  function.
C
C Input:  OMEGA      output shaft rotation rate  radians/s
C         VOLT       terminal voltage (or throttle for IC motors)
C         IMOTYPE    specifies type of motor model to be used
C         PARMOT(.)  motor parameters  (lines 3,4... in motor file)
C         NMPAR      number of motor parameters in motor file
C
C Output: Q          output shaft torque   (N-m)
C         Q_OMEGA    dQ/dOMEGA  function derivative
C         Q_VOLT     dQ/dVOLT   function derivative
C         I          motor current (or fuel flow for IC motors)
C         I_OMEGA    dI/dOMEGA  function derivative
C         I_VOLT     dI/dVOLT   function derivative
C
C-----------------------------------------------------------
      REAL PARMOT(2,*)
      REAL I, I_OMEGA, I_VOLT
C
      REAL KVRPM, KQRPM, KVRAD, KQRAD
      DATA PI / 3.141592653589793238 /
      DATA EPS / 1.0E-6 /
C
C===================================================================
      IF(IMOTYPE.EQ.1) THEN
C----- Brushed DC motor - 1st-order model
       IF(NMPAR.LT.3) THEN
        WRITE(*,*) 'MOTORQ: Motor model 1 needs  3  parameters.',
     &             '  Number passed in:', NMPAR
        STOP
       ENDIF
C
       RMOTOR = PARMOT(1,1)    ! R   (Ohms)      motor resistance
       ZLOADI = PARMOT(1,2)    ! Io  (Amps)      zero-load current
       KVRPM  = PARMOT(1,3)    ! Kv  (rpm/Volt)  motor constant
C
       KVRAD = KVRPM * PI/30.0
       KQRAD = KVRAD
C     
       VM       = OMEGA/KVRAD
       VM_OMEGA = 1.0  /KVRAD
C
       I       = (VOLT - VM      )/RMOTOR
       I_OMEGA =       - VM_OMEGA /RMOTOR
       I_VOLT  =  1.0             /RMOTOR
C
       Q       = (I       - ZLOADI)/KQRAD
       Q_OMEGA =  I_OMEGA          /KQRAD
       Q_VOLT  =  I_VOLT           /KQRAD
C
C===================================================================
      ELSEIF(IMOTYPE.EQ.2) THEN
C----- Brushed DC motor - 2nd-order model
       IF(NMPAR.LT.3) THEN
        WRITE(*,*) 'MOTORQ: Motor model 2 needs at least 3 parameters.',
     &             '  Number passed in:', NMPAR
        STOP
       ENDIF
C
       RMOTOR0 = PARMOT(1,1)    ! R0  (Ohms)      motor resistance
       ZLOADI0 = PARMOT(1,2)    ! Io0 (Amps)      zero-load current
       KVRPM   = PARMOT(1,3)    ! Kv  (rpm/Volt)  motor constant
       KQRPM   = PARMOT(1,4)    ! Kq  (rpm/Volt)  motor constant
       TAU     = PARMOT(1,5)    ! tau
       ZLOADI1 = PARMOT(1,6)    ! Io1
       ZLOADI2 = PARMOT(1,7)    ! Io2
       RMOTOR2 = PARMOT(1,8)    ! R2  (Ohms/Amp^2)
C
C----- default case for Kq is Kq=Kv
       IF(KQRPM .EQ. 0.0) KQRPM = KVRPM
C
       KVRAD = KVRPM * PI/30.0
       KQRAD = KQRPM * PI/30.0
C
       VM       = (1.0 + TAU*OMEGA    )*OMEGA/KVRAD
       VM_OMEGA = (1.0 + TAU*OMEGA*2.0)      /KVRAD
C
       I = (VOLT - VM)/RMOTOR0
       DO ITER = 1, 10
         RES = I*(RMOTOR0 + RMOTOR2*I**2) + VM - VOLT
         RES_I = RMOTOR0 + 3.0*RMOTOR2*I**2
         I = I - RES/RES_I
         IF(ABS(RES) .LT. EPS*MAX(1.0,ABS(VOLT))) GO TO 11
       ENDDO
       WRITE(*,*) 'MOTOR: Current convergence failed'
 11    CONTINUE
       RES_OMEGA = VM_OMEGA
       RES_VOLT  = -1.0
       I_OMEGA = -RES_OMEGA/RES_I
       I_VOLT  = -RES_VOLT /RES_I
C
       ZLOADI       = ZLOADI0 + ZLOADI1*OMEGA + ZLOADI2*OMEGA**2
       ZLOADI_OMEGA =           ZLOADI1       + ZLOADI2*OMEGA * 2.0
C
       Q       = (I       - ZLOADI      ) / KQRAD
       Q_OMEGA = (I_OMEGA - ZLOADI_OMEGA) / KQRAD
       Q_VOLT  =  I_VOLT                  / KQRAD
C
C===================================================================Added by Murat Bronz
C      IF(IMOTYPE.EQ.4) THEN
C----- Brushed DC motor - 1st-order model with the ESC effect included through internal resistance.
C       IF(NMPAR.LT.6) THEN
C        WRITE(*,*) 'MOTORQ: Motor model 4 needs  6????  parameters.',
C     &             '  Number passed in:', NMPAR
C        STOP
C       ENDIF
C
C       RMOTOR = PARMOT(1,1)    ! R   (Ohms)      motor resistance
C       ZLOADI = PARMOT(1,2)    ! Io  (Amps)      zero-load current
C       KVRPM  = PARMOT(1,3)    ! Kv  (rpm/Volt)  motor constant
C
C       KVRAD = KVRPM * PI/30.0
C       KQRAD = KVRAD
C     
C       VM       = OMEGA/KVRAD
C       VM_OMEGA = 1.0  /KVRAD
C
C       I       = (VOLT - VM      )/RMOTOR
C       I_OMEGA =       - VM_OMEGA /RMOTOR
C       I_VOLT  =  1.0             /RMOTOR
C
C       Q       = (I       - ZLOADI)/KQRAD
C       Q_OMEGA =  I_OMEGA          /KQRAD
C       Q_VOLT  =  I_VOLT           /KQRAD
C
C===================================================================Finished MB
      ELSEIF(IMOTYPE.EQ.11 .OR.
     &       IMOTYPE.EQ.12      ) THEN
C----- IMOTYPE=11: IC motor, tabulated Q(w,throttle) curves
C----- IMOTYPE=12: IC motor, tabulated P(w,throttle) curves
       IF(NMPAR.LT.3) THEN
        WRITE(*,*)
     &   'MOTORQ: Motor model 11 or 12 needs at least 5 parameters.',
     &         '  Number passed in:', NMPAR
        STOP
       ENDIF
C
       NRPM = INT( PARMOT(1,1) + 0.001 )
       NTHR = INT( PARMOT(2,1) + 0.001 )
       NTHR = MAX( NTHR , 1 )
C
       RPMSPEC = OMEGA * 30.0/PI
       THRSPEC = VOLT
C
       L0 = 1
       IF(NTHR .EQ. 1) THEN
C------ only one throttle line given... assume throttle runs from 0 to THR(1)
        ITHR = 1
        L = L0 + ITHR
        THR1 = 0.0
        THR2 = PARMOT(1,L)
C
       ELSE
C------ two or more throttle lines... find THR(*) interval containing THRSPEC
        DO ITHR = 1, NTHR-1
          ITHR1 = ITHR
          ITHR2 = ITHR + 1
          L1 = L0 + ITHR1
          L2 = L0 + ITHR2
          THR1 = PARMOT(1,L1)
          THR2 = PARMOT(1,L2)
          IF(THRSPEC .LE. THR2) GO TO 20
        ENDDO
C
       ENDIF
C
 20    CONTINUE
C----- set throttle-interpolation fractions 
       DTHR = THR2 - THR1
       IF(DTHR .EQ. 0.0) THEN
        TFRAC1 = 0.0
        TFRAC2 = 1.0
        TFRAC1_THR = 0.
        TFRAC2_THR = 0.
       ELSE
        TFRAC1 = (THR2 - THRSPEC) / DTHR
        TFRAC2 = (THRSPEC - THR1) / DTHR
        TFRAC1_THR = -1.0/DTHR
        TFRAC2_THR =  1.0/DTHR
       ENDIF
C
C----- RPM, Q or P  scaling factors
       L0 = 1 + NTHR
       L = L0 + 1
       RPMFAC = PARMOT(1,L)
       PARFAC = PARMOT(2,L)
C
C----- find RPM interval containing RPMSPEC
       L0 = 1 + NTHR + 1
       DO IRPM = 1, NRPM-1
         L1 = L0 + IRPM + (ITHR1-1)*NRPM
         L2 = L0 + IRPM + (ITHR2-1)*NRPM
C
         RPM1 = RPMFAC*(  PARMOT(1,L1  )*TFRAC1
     &                  + PARMOT(1,L2  )*TFRAC2 )
         RPM2 = RPMFAC*(  PARMOT(1,L1+1)*TFRAC1
     &                  + PARMOT(1,L2+1)*TFRAC2 )
C
         IF(RPMSPEC .LE. RPM2) GO TO 22
       ENDDO
 22    CONTINUE
C
C----- set Q or P at ends of RPM interval by interpolating between throttle lines
       PAR1     = PARFAC*(  PARMOT(2,L1  )*TFRAC1
     &                    + PARMOT(2,L2  )*TFRAC2 )
       PAR2     = PARFAC*(  PARMOT(2,L1+1)*TFRAC1
     &                    + PARMOT(2,L2+1)*TFRAC2 )
       PAR1_THR = PARFAC*(  PARMOT(1,L1  )*TFRAC1_THR
     &                    + PARMOT(1,L2  )*TFRAC2_THR )
       PAR2_THR = PARFAC*(  PARMOT(1,L1+1)*TFRAC1_THR
     &                    + PARMOT(1,L2+1)*TFRAC2_THR )
C
C----- set RPM-interpolation fractions
       DRPM = RPM2 - RPM1
       IF(DRPM .EQ. 0.0) THEN
        RFRAC1 = 0.0
        RFRAC2 = 1.0
        RFRAC1_RPM = 0.
        RFRAC2_RPM = 0.
       ELSE
        RFRAC1 = (RPM2 - RPMSPEC) / DRPM
        RFRAC2 = (RPMSPEC - RPM1) / DRPM
        RFRAC1_RPM = -1.0/DRPM
        RFRAC2_RPM =  1.0/DRPM
       ENDIF
C
C----- interpolate PAR to RPMSPEC
       PAR     = PAR1    *RFRAC1     + PAR2    *RFRAC2
       PAR_RPM = PAR1    *RFRAC1_RPM + PAR2    *RFRAC2_RPM
       PAR_THR = PAR1_THR*RFRAC1     + PAR2_THR*RFRAC2
C
       IF(IMOTYPE.EQ.11) THEN
C------ PAR is Q (torque)
        Q       = PAR
        Q_OMEGA = PAR_RPM * 30.0/PI
        Q_VOLT  = PAR_THR
C
        I       = 0.
        I_OMEGA = 0.
        I_VOLT  = 0.
C
       ELSE
C------ PAR is P (power)
        Q       = PAR    /OMEGA
        Q_OMEGA = PAR_RPM/OMEGA * 30.0/PI  -  Q/OMEGA
        Q_VOLT  = PAR_THR/OMEGA
C
        I       = 0.
        I_OMEGA = 0.
        I_VOLT  = 0.
C
       ENDIF
C
C===================================================================
      ELSE
C----- Other motor models would go here
       WRITE(*,*) 'MOTORQ: Undefined motor type index:', IMOTYPE
       STOP
      ENDIF
C===================================================================
C
      RETURN
      END ! MOTORQ



      SUBROUTINE VOLTM(  OMEGA,      Q, IMOTYPE, PARMOT,NMPAR, 
     &        VOLT, VOLT_OMEGA, VOLT_Q, 
     &        AMPS, AMPS_OMEGA, AMPS_Q )
C-----------------------------------------------------------
C     Motor+gearbox   Voltage(rpm,torque)  function.
C     Inverts MOTORQ's torque(rpm,Voltage)  function
C       via Newton iteration.
C
C Input:  OMEGA      output shaft rotation rate  (radians/s)
C         Q          output shaft torque   (N-m)
C         IMOTYPE    specifies type of motor model to be used
C         PARMOT(.)  motor parameters  (lines 3,4... in motor file)
C         NMPAR      number of motor parameters in motor file
C
C Output: VOLT       terminal voltage
C         VOLT_OMEGA dVOLT/dOMEGA  function derivative
C         VOLT_Q     dVOLT/dQ      function derivative
C         AMPS       current
C         AMPS_OMEGA dAMPS/dOMEGA  function derivative
C         AMPS_Q     dAMPS/dQ      function derivative
C
C-----------------------------------------------------------
      REAL PARMOT(2,*)
      REAL KVRPM, KQRPM, KVRAD, KQRAD
C
      DATA PI / 3.141592653589793238 /
      DATA EPS / 1.0E-6 /
C
C---- Initial guess for Newton iteration, for each IMOTYPE
C-    May need to be different for different IMOTYPE.
      IF(IMOTYPE.EQ.1) THEN
       RMOTOR = PARMOT(1,1)    ! R   (Ohms)      motor resistance
       ZLOADI = PARMOT(1,2)    ! Io  (Amps)      zero-load current
       KVRPM  = PARMOT(1,3)    ! Kv  (rpm/Volt)  motor constant
       KVRAD = KVRPM*PI/30.0
       KQRAD = KVRAD
       AMPS = Q*KQRAD + ZLOADI
       VOLT = AMPS*RMOTOR + OMEGA/KVRAD
      ELSEIF(IMOTYPE.EQ.2) THEN
       RMOTOR = PARMOT(1,1)    ! R   (Ohms)      motor resistance
       ZLOADI = PARMOT(1,2)    ! Io  (Amps)      zero-load current
       KVRPM  = PARMOT(1,3)    ! Kv  (rpm/Volt)  motor constant
       KVRAD = KVRPM*PI/30.0
       KQRAD = KVRAD
       AMPS = Q*KQRAD + ZLOADI
       VOLT = AMPS*RMOTOR + OMEGA/KVRAD
      ELSE
C----- default case
       VOLT = 1.0
      ENDIF
C
      DO ITER = 1, 20
        CALL MOTORQ(OMEGA,VOLT, IMOTYPE, PARMOT,NMPAR, 
     &              QM,QM_OMEGA,QM_VOLT,
     &              AM,AM_OMEGA,AM_VOLT )
C
        RES      = QM - Q
        RES_VOLT = QM_VOLT
C
        IF(RES_VOLT .EQ. 0.0) GO TO 50
C
        DVOLT = -RES/RES_VOLT
ccc        write(*,*) iter, dvolt
        IF(ABS(DVOLT) .LT. EPS*MAX(1.0,ABS(VOLT))) GO TO 20
C
        VOLT = VOLT + DVOLT
      ENDDO
 10   CONTINUE
      WRITE(*,*) '** VOLTM: Voltage convergence failed. Res =', RES
C
 20   CONTINUE
      RES_OMEGA = QM_OMEGA
      RES_Q     = -1.0
C
      VOLT_OMEGA = -RES_OMEGA / RES_VOLT
      VOLT_Q     = -RES_Q     / RES_VOLT
C
      AMPS_OMEGA = AM_VOLT*VOLT_OMEGA + AM_OMEGA
      AMPS_Q     = AM_VOLT*VOLT_Q
C
      RETURN
C
 50   CONTINUE
      RETURN
C
      END ! VOLTM

