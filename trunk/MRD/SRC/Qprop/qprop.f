C***********************************************************************
C    Module:  qprop.f
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

      PROGRAM QPROP
C--------------------------------------------------------
C     Propeller/motor performance program
C     Version 1.30   25 Mar 09
C
C     Usage:
C
C % qprop propfile motorfile Vel Rpm Volt dBeta    (single-point)
C
C % qprop propfile motorfile Vel1,Vel2,dVel Rpm             (1-param multi-point)
C
C % qprop propfile motorfile Vel1,Vel2/NVel  -  Volt dBeta  (1-param multi-point)
C
C % qprop propfile motorfile Vel1,Vel2/NVel  -  Volt1,Volt2,dVolt dBeta  
C                                                           (2-param multi-point)
C
C--------------------------------------------------------
      IMPLICIT REAL (A-H,M,O-Z)
C
C---- input radial quantities (from propfile)
      PARAMETER (IRDIM=81)
      REAL WORK(IRDIM)
      REAL RB(IRDIM), CB(IRDIM), BB(IRDIM)
      REAL CL0B(IRDIM), DCLDAB(IRDIM), CLMINB(IRDIM), CLMAXB(IRDIM)
      REAL CD0B(IRDIM), CD2UB(IRDIM), CD2LB(IRDIM), CLCD0B(IRDIM)
      REAL REREFB(IRDIM), REEXPB(IRDIM), MCRITB(IRDIM)
C
C---- radial quantities interpolated to computational stations
      PARAMETER (IDIM=25)
      REAL R(IDIM), C(IDIM), B(IDIM), DR(IDIM)
      REAL CL0(IDIM), DCLDA(IDIM), CLMIN(IDIM), CLMAX(IDIM)
      REAL CD0(IDIM), CD2U(IDIM), CD2L(IDIM), CLCD0(IDIM)
      REAL REREF(IDIM), REEXP(IDIM), MCRIT(IDIM)
      REAL VA(IDIM), VT(IDIM), CL(IDIM), CD(IDIM)
      LOGICAL STALL(IDIM)
C
      REAL TP_C(IDIM), TP_B(IDIM),
     &     QP_C(IDIM), QP_B(IDIM)
C
C---- motor parameters
      PARAMETER (NMPDIM=500)
      REAL PARMOT(2,NMPDIM)
      INTEGER NMPLIN(NMPDIM)
      CHARACTER*32 PMLAB(NMPDIM)
C
C---- various character variables
      CHARACTER*1 CHARF, ANS
      CHARACTER*80 PNAME, MNAME
      CHARACTER*80 ARGP1, ARGP2, ARGP3, ARGP4, ARGP5,
     &             ARGP6, ARGP7, ARGP8, ARGP9, ARGP10
      CHARACTER*80 FILNAM
      CHARACTER*128 LINE
C
      LOGICAL LRDUMP
      LOGICAL ERROR
C
C---- parameter pointers
      PARAMETER(IPVEL  = 1,
     &          IPRPM  = 2,
     &          IPVOLT = 3,
     &          IPDBET = 4,
     &          IPTHRU = 5,
     &          IPTORQ = 6,
     &          IPAMPS = 7,
     &          IPPELE = 8,
     &          IPTOT  = 8 )
      REAL PAR(IPTOT),
     &     PAR1(IPTOT), 
     &     PAR2(IPTOT),
     &     DPAR(IPTOT)
      INTEGER NPAR(IPTOT)
      LOGICAL LPAR(IPTOT)
      CHARACTER*6 PARNAME(IPTOT)
      INTEGER IPSWEEP(IPTOT), NSWEEP
C
      REAL ASYS(4,4), RES(4)
C
C---- input-receiving arrays
      REAL RVAL(15)
      INTEGER IVAL(15)
C
      LOGICAL LQMATCH
C
      INCLUDE 'QDEF.INC'
C
      DATA PARNAME /
     &  'Vel   ' ,
     &  'Rpm   ' ,
     &  'Volt  ' ,
     &  'Dbeta ' ,
     &  'Thrust' ,
     &  'Torque' ,
     &  'Amps  ' ,
     &  'Pele  '   /
C   
      DATA PI / 3.141592653589793238 /
ccc      DATA EPS / 1.0E-6 /
      DATA EPS / 1.0E-8 /
C
      DATA VERSION / 1.31 /
C
C---- default Mcrit
      MCRIT0 = 0.70
C
C---- get Unix command-line arguments, if any
      CALL GETARG0(1,ARGP1)
      CALL GETARG0(2,ARGP2)
      CALL GETARG0(3,ARGP3)
      CALL GETARG0(4,ARGP4)
      CALL GETARG0(5,ARGP5)
      CALL GETARG0(6,ARGP6)
      CALL GETARG0(7,ARGP7)
      CALL GETARG0(8,ARGP8)
      CALL GETARG0(9,ARGP9)
      CALL GETARG0(10,ARGP10)
C
      IF(ARGP1.EQ.' ') THEN
       WRITE(*,1005)
 1005  FORMAT(
     & /' QPROP usage (parameters in brackets are optional):'
     &//' % qprop propfile motorfile Vel Rpm ',
     &             ' [ Volt dBeta Thrust Torque Amps Pele ]',
     &  '   (single-point)'
     &//' % qprop propfile motorfile Vel1,Vel2,dVel Rpm             ',
     &  '   (multi-point 1-parameter sweep over Vel, Rpm set)'
     &//' % qprop propfile motorfile Vel1,Vel2/nVel Rpm             ',
     &  '   (multi-point 1-parameter sweep over Vel, Rpm set)'
     &//' % qprop propfile motorfile Vel1,Vel2,dVel  -  Volt        ',
     &  '   (multi-point 1-parameter sweep over Vel, Volt set)'
     &//' % qprop propfile motorfile Vel1,Vel2,dVel Rpm1,Rpm2,dRpm  ',
     &  '   (multi-point 2-parameter sweep over Vel and Rpm)'
     &       )
       WRITE(*,*)
       WRITE(*,*) 'Run with default inputs?  Y'
       READ(*,1000) ANS
       IF(INDEX('Nn',ANS) .NE. 0) STOP
       WRITE(*,*) 
      ENDIF
C
C---- default fluid properties from QDEF.INC
      RHO = RHO1   ! density
      RMU = RMU1   ! viscosity
      VSO = VSO1   ! speed of sound
C
      CALL QCGET(RHO,RMU,VSO)
C
C==========================================================
C---- set default prop
      PNAME = 'Graupner CAM 6x3 folder'
      BLDS = 2.0
C
C---- number of radial stations
      NR = 7
C
C---- linear CL(alpha) function
C     CL  =  CL0 + DCLCD*alpha  ,  clipped if outside range  CLMIN..CLMAX
      DO IR = 1, NR
        CL0B(IR) = 0.5
        DCLDAB(IR) = 5.8
        CLMINB(IR) = -0.4
        CLMAXB(IR) = 1.2
      ENDDO
C
C---- quadratic CD(CL,Re) function
C     CD  =  [ CD0 + CD2*(CL-CLCD0)**2 ] * [Re/REREF]^REEXP
      DO IR = 1, NR
        CD0B(IR) = 0.028
        CD2UB(IR) = 0.050
        CD2LB(IR) = 0.050
        CLCD0B(IR) = 0.5
        REREFB(IR) = 70000.0
        REEXPB(IR) = -0.7
        MCRITB(IR) = MCRIT0
      ENDDO
C
C---- radii
      RFAC = 0.0254
      RADD = 0.
      RB(1) = 0.75  
      RB(2) = 1.00  
      RB(3) = 1.50  
      RB(4) = 2.00  
      RB(5) = 2.50  
      RB(6) = 2.875 
      RB(7) = 3.00  
C
C---- chords
      CFAC = 0.0254
      CADD = 0.
      CB(1) = 0.66 
      CB(2) = 0.69 
      CB(3) = 0.63 
      CB(4) = 0.55 
      CB(5) = 0.44 
      CB(6) = 0.30 
      CB(7) = 0.19 
C
C---- blade angles
      BFAC = 1.0
      BADD = 0.
      BB(1) = 27.5
      BB(2) = 22.0
      BB(3) = 15.2
      BB(4) = 10.2
      BB(5) =  6.5
      BB(6) =  4.6
      BB(7) =  4.2
C
      RAD = RB(NR)
C
C----------------------------------------------------
C---- default motor/gear combo
      MNAME = "Speed-400 3321 (6V) direct drive"
      IMOTYPE = 1
      PARMOT(1,1) = 0.31    ! Rmotor  (Ohms)
      PARMOT(1,2) = 0.77    ! Io      (Amps)
      PARMOT(1,3) = 2760.0  ! Kv      (rpm/Volt)
      NMPLIN(1) = 1
      NMPLIN(2) = 1
      NMPLIN(3) = 1
      PMLAB(1) = 'R  (Ohm)'
      PMLAB(2) = 'Io (Amp)'
      PMLAB(3) = 'Kv (rpm/Volt)'
      NMPAR = 3
C
C----------------------------------------------------
C---- default parameter sweeps
      DO IP = 1, IPTOT
        PAR1(IP) = 0.
        PAR2(IP) = 0.
        NPAR(IP) = 0
      ENDDO
C
      NSWEEP = 2
C
      IPSWEEP(1) = IPVEL
      PAR1(IPVEL) =  0.0
      PAR2(IPVEL) = 10.0
      NPAR(IPVEL) = 6
C
      IPSWEEP(2) = IPRPM
      PAR1(IPRPM) = 10000.0
      PAR2(IPRPM) = 16000.0
      NPAR(IPRPM) = 7
C
      NPAR(IPVOLT) = 1
      NPAR(IPDBET) = 1

C==========================================================
 1000 FORMAT(A)
C
C----------------------------------------------------
C---- read prop data file
      FILNAM = ARGP1
      IF(FILNAM.EQ.' ') GO TO 18
C
      LU = 1
      OPEN(LU,FILE=FILNAM,STATUS='OLD',ERR=18)
C
      ILINE = 0
C
C---- prop name
      CALL FREAD(LU,LINE,ILINE,IERR,PNAME)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
C
C---- extract parameters on data lines
      NVAL = 2
      CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
      IF(NVAL.LT. 1) GO TO 980
      BLDS = RVAL(1)
      IF(NVAL.GE.2) THEN
       RAD = RVAL(2)
      ELSE
       RAD = 0.
      ENDIF
C
      NVAL = 2
      CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
      IF(NVAL.LT. 2) GO TO 980
      DO IR = 1, IRDIM
        CL0B(IR)   = RVAL(1)
        DCLDAB(IR) = RVAL(2)
      ENDDO
C
      NVAL = 2
      CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
      IF(NVAL.LT. 2) GO TO 980
      DO IR = 1, IRDIM
        CLMINB(IR) = RVAL(1)
        CLMAXB(IR) = RVAL(2)
      ENDDO
C
      NVAL = 4
      CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
      IF(NVAL.LT. 3) GO TO 980
      DO IR = 1, IRDIM
        CD0B(IR)   = RVAL(1)
        CD2UB(IR)  = RVAL(2)
        CD2LB(IR)  = RVAL(3)
        CLCD0B(IR) = RVAL(4)
      ENDDO
C
      NVAL = 2
      CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
      IF(NVAL.LT. 2) GO TO 980
      DO IR = 1, IRDIM
        REREFB(IR) = RVAL(1)
        REEXPB(IR) = RVAL(2)
      ENDDO
C
C
      NVAL = 3
      CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
      IF(NVAL.LT. 3) GO TO 980
      RFAC = RVAL(1)
      CFAC = RVAL(2)
      BFAC = RVAL(3)
C
      NVAL = 3
      CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
      IF(NVAL.LT. 3) GO TO 980
      RADD = RVAL(1)
      CADD = RVAL(2)
      BADD = RVAL(3)
C
      KR = 0
C
 14   CONTINUE
C
        NVAL = 13
        CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
        IF(IERR.EQ.+1) GO TO 900
        IF(IERR.EQ.-1) GO TO 16
        IF(NVAL.LT. 3) GO TO 980
C
        KR = KR + 1
        IR = MIN( KR , IRDIM )
        RB(IR) = RVAL(1)
        CB(IR) = RVAL(2)
        BB(IR) = RVAL(3)
C
        MCRITB(IR) = MCRIT0
C
        IF(NVAL.GE. 4) CL0B(IR)   = RVAL( 4)
        IF(NVAL.GE. 5) DCLDAB(IR) = RVAL( 5)
        IF(NVAL.GE. 6) CLMINB(IR) = RVAL( 6)
        IF(NVAL.GE. 7) CLMAXB(IR) = RVAL( 7)
        IF(NVAL.GE. 8) CD0B(IR)   = RVAL( 8)
        IF(NVAL.GE. 9) CD2UB(IR)  = RVAL( 9)
        IF(NVAL.GE.10) CD2LB(IR)  = RVAL(10)
        IF(NVAL.GE.11) CLCD0B(IR) = RVAL(11)
        IF(NVAL.GE.12) REREFB(IR) = RVAL(12)
        IF(NVAL.GE.13) REEXPB(IR) = RVAL(13)
C
        GO TO 14
C
 16   CONTINUE
      CLOSE(LU)
C
      IF(KR.GT.0) THEN
       NR = IR
       IF(KR.GT.NR) THEN
        WRITE(*,*) 'Array overflow.  Increase IRDIM to', KR
        STOP
       ENDIF
      ENDIF
      GO TO 19
C
 18   CONTINUE
      WRITE(*,*)
      WRITE(*,*) 'Prop file not found:  ', FILNAM(1:48)
      WRITE(*,*) 'Default prop used'
C
C
 19   CONTINUE
C
      IF(NR.LE.1) THEN
       WRITE(*,*)
       WRITE(*,*) '*** Must define at least two radial stations'
       STOP
      ENDIF
C
C---- apply scaling factors
      DO IR = 1, NR
        RB(IR) =  RB(IR)*RFAC + RADD
        CB(IR) =  CB(IR)*CFAC + CADD
        BB(IR) = (BB(IR)*BFAC + BADD)* PI / 180.0
      ENDDO
C
      IF(RAD .EQ. 0.0) THEN
       RAD = RB(NR)
      ENDIF
C
      DO IR = 1, NR-1
        IF(CB(IR) .LE. 0.0) 
     &     STOP 'Chords must be positive'
        IF(RB(IR) .LT. 0.0)
     &     STOP 'Radii must be nonnegative'
        IF(RB(IR) .GE. RB(IR+1))
     &     STOP 'Radii must increase monotonically'
      ENDDO
C
      IF(RAD .LT. RB(NR)) THEN
       WRITE(*,1050) RAD, RB(NR)
 1050  FORMAT(/' Given on line 2:  R =', G12.4,
     &        /' Last r station :  r =', G12.4,
     &       //' Must have  R > r' / )
       STOP
      ENDIF
C
C==========================================================
C---- read motor data file
      FILNAM = ARGP2
      IF(FILNAM.EQ.' ') GO TO 28
C
      LU = 2
      OPEN(LU,FILE=FILNAM,STATUS='OLD',ERR=28)
C
C---- clear motor data in case it's not all in the file
      DO IMPAR = 1, NMPDIM
        PMLAB(IMPAR) = ' '
        PARMOT(1,IMPAR) = 0.0
        PARMOT(2,IMPAR) = 0.0
        NMPLIN(IMPAR) = 0
      ENDDO
C
      ILINE = 0
C
C---- motor name
      CALL FREAD(LU,LINE,ILINE,IERR,MNAME)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
C
C---- motor model index
      NVAL = 1
      CALL IREAD(LU,LINE,ILINE,IERR,NVAL,IVAL)
      IF(IERR.EQ.+1) GO TO 900
      IF(IERR.EQ.-1) GO TO 950
      IF(NVAL.LT. 1) GO TO 980
      IMOTYPE = IVAL(1)
C
C---- extract parameters on data lines
      DO IMPAR = 1, NMPDIM+1
        NVAL = 2
        RVAL(1) = 0.
        RVAL(2) = 0.
        CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
        IF(IERR.EQ.+1) GO TO 900
        IF(IERR.EQ.-1) GO TO 25
        IF(NVAL.LT. 1) GO TO 980
        IF(IMPAR.EQ.NMPDIM+1) THEN
         WRITE(*,*) '* Motor parameter array overflow. Increase NMPDIM'
         STOP
        ENDIF
        PARMOT(1,IMPAR) = RVAL(1)
        PARMOT(2,IMPAR) = RVAL(2)
        NMPLIN(IMPAR) = NVAL
C
        KEX = INDEX(LINE,'!')
        IF(KEX.GE.1) THEN
         PMLAB(IMPAR) = LINE(KEX+1:80)
        ELSE
         PMLAB(IMPAR) = ' '
        ENDIF
      ENDDO
C
 25   CONTINUE
      NMPAR = IMPAR-1
C
      CLOSE(LU)
      GO TO 29
C
 28   CONTINUE
      WRITE(*,*)
      WRITE(*,*) 'Motor file not found:  ', FILNAM(1:48)
      WRITE(*,*) 'Default motor used'
C
 29   CONTINUE
C
C==========================================================
C---- operating parameter data file, or single-point parameters
      FILNAM = ARGP3
      IF(FILNAM.EQ.' ') GO TO 80
C
C---- default parameters
      DO IP = 1, IPTOT
        PAR1(IP) = 0.
        PAR2(IP) = 0.
        NPAR(IP) = 0
      ENDDO
C
      DO IP = 1, 4
        NPAR(IP) = MAX( 1 , NPAR(IP) )
      ENDDO
C
C---- try reading velocity 3rd Unix argument
      IP = IPVEL
      CALL PPARSE(ARGP3,PAR1(IP),PAR2(IP),NPAR(IP),IERR)
      IF(IERR.EQ.+1 .OR.
     &   IERR.EQ.-1      ) THEN
       WRITE(*,*)
       WRITE(*,*) 'Run parameters not specified'
       WRITE(*,*) 'Default velocities, voltages, pitch used'
       GO TO 80
      ENDIF
C
C---- try reading remaining parameters from remaining Unix arguments, if any
      IP = IPRPM
      CALL PPARSE(ARGP4,PAR1(IP),PAR2(IP),NPAR(IP),IERR)
C
      IP = IPVOLT
      CALL PPARSE(ARGP5,PAR1(IP),PAR2(IP),NPAR(IP),IERR)
C
      IP = IPDBET
      CALL PPARSE(ARGP6,PAR1(IP),PAR2(IP),NPAR(IP),IERR)
C
      IP = IPTHRU
      CALL PPARSE(ARGP7,PAR1(IP),PAR2(IP),NPAR(IP),IERR)
C
      IP = IPTORQ
      CALL PPARSE(ARGP8,PAR1(IP),PAR2(IP),NPAR(IP),IERR)
C
      IP = IPAMPS
      CALL PPARSE(ARGP9,PAR1(IP),PAR2(IP),NPAR(IP),IERR)
C
      IP = IPPELE
      CALL PPARSE(ARGP10,PAR1(IP),PAR2(IP),NPAR(IP),IERR)
C
C---- find which parameter(s) are to have have multiple values
      DO KP = 1, IPTOT
        IPSWEEP(KP) = 0
      ENDDO
C
      NSWEEP = 0
      DO IP = 1, IPTOT
        IF(NPAR(IP) .GT. 1) THEN
         NSWEEP = NSWEEP + 1
         IPSWEEP(NSWEEP) = IP
        ENDIF
      ENDDO
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C---- pick up here after reading everything
 80   CONTINUE
C
C---- if this is a single-point case... will dump radial distributions
      LRDUMP = NSWEEP .EQ. 0
C
C
C==========================================================
C
C---- set up finely-spaced radial arrays
      R0 = RB(1)
      R1 = RB(NR)
C
      N = IDIM
      DO I = 1, N
        FRAC = (FLOAT(I)-0.5)/FLOAT(N)
        R(I) = R0*(1.0-FRAC) + R1*FRAC
        DR(I) = (R1-R0)/FLOAT(N)
      ENDDO
C
      CALL SPLINE(CB,WORK,RB,NR)
      DO I = 1, N
        C(I) = SEVAL(R(I),CB,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(BB,WORK,RB,NR)
      DO I = 1, N
        B(I) = SEVAL(R(I),BB,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(CL0B,WORK,RB,NR)
      DO I = 1, N
        CL0(I) = SEVAL(R(I),CL0B,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(DCLDAB,WORK,RB,NR)
      DO I = 1, N
        DCLDA(I) = SEVAL(R(I),DCLDAB,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(CLMINB,WORK,RB,NR)
      DO I = 1, N
        CLMIN(I) = SEVAL(R(I),CLMINB,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(CLMAXB,WORK,RB,NR)
      DO I = 1, N
        CLMAX(I) = SEVAL(R(I),CLMAXB,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(CD0B,WORK,RB,NR)
      DO I = 1, N
        CD0(I) = SEVAL(R(I),CD0B,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(CD2UB,WORK,RB,NR)
      DO I = 1, N 
        CD2U(I) = SEVAL(R(I),CD2UB,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(CD2LB,WORK,RB,NR)
      DO I = 1, N
        CD2L(I) = SEVAL(R(I),CD2LB,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(CLCD0B,WORK,RB,NR)
      DO I = 1, N
        CLCD0(I) = SEVAL(R(I),CLCD0B,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(REREFB,WORK,RB,NR)
      DO I = 1, N
        REREF(I) = SEVAL(R(I),REREFB,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(REEXPB,WORK,RB,NR)
      DO I = 1, N
        REEXP(I) = SEVAL(R(I),REEXPB,WORK,RB,NR)
      ENDDO
C
      CALL SPLINE(MCRITB,WORK,RB,NR)
      DO I = 1, N
        MCRIT(I) = SEVAL(R(I),MCRITB,WORK,RB,NR)
      ENDDO
C
C---- reality checks
      ERROR = .FALSE.
      DO I = 1, N
        IF(C(I) .LE. 0.0) THEN
         WRITE(*,*) 'Negative chord at i =', I
         ERROR = .TRUE.
        ENDIF
        IF(REREF(I) .LE. 0.0) THEN
         WRITE(*,*) 'Negative Re_ref at i =', I
         ERROR = .TRUE.
        ENDIF
        IF(MCRIT(I) .LE. 0.0) THEN
         WRITE(*,*) 'Negative Mcrit at i =', I
         ERROR = .TRUE.
        ENDIF
      ENDDO
C
      IF(ERROR) THEN
        WRITE(*,*)
       WRITE(*,1100)
     & ' i   radius   chord     beta    Re_ref'
        DO I = 1, N
          IRE = INT( REREF(I) )
          WRITE(*,1070) I, R(I), C(I), B(I)*180.0/PI, IRE
 1070     FORMAT(1X,I3, F9.4, F9.4, F9.3, I9)
        ENDDO
        WRITE(*,*)
        STOP
      ENDIF

C
C----------------------------------------------------
C---- perform calculations and dump output
C
      LU = 6
      WRITE(*,*)
C
 1105 FORMAT('# QPROP Version', F5.2)
 1100 FORMAT('# ', A,A,A,A)
 1110 FORMAT('#  ', G12.5, 1X, A)
 1120 FORMAT('#   rho =', G12.5,' kg/m^3'
     &      /'#   mu  =', G12.5,' kg/m-s'
     &      /'#   a   =', G12.5,' m/s   ' )
C
      WRITE(LU,1105) VERSION
      WRITE(LU,1100)
      WRITE(LU,1100) PNAME
      WRITE(LU,1100)
      WRITE(LU,1100) MNAME
      IF(IMOTYPE .LT. 10) THEN
       DO IMPAR=1, NMPAR
         WRITE(LU,1110) PARMOT(1,IMPAR), PMLAB(IMPAR)
       ENDDO
      ENDIF
      WRITE(LU,1100)
      WRITE(LU,1120) RHO, RMU, VSO
      WRITE(LU,1100)
      WRITE(LU,1100)
     & ' 1         2        3          4          5        ',
     & ' 6            7         8       9        10        11    ',
     & '    12          13        14        15      16          17   ',
     & '        18      19'
      WRITE(LU,1100)
      WRITE(LU,1100)
     & ' V(m/s)    rpm      Dbeta      T(N)       Q(N-m)   ',
     & ' Pshaft(W)    Volts     Amps    effmot   effprop   adv   ',
     & '    CT          CP        DV(m/s)   eff     Pelec       Pprop',
     & '        cl_avg  cd_avg'
C
      IF(LRDUMP) THEN
       CHARF = '#'
      ELSE
       CHARF = ' '
      ENDIF
C
C
      DO IP = 1, IPTOT
        NPM = MAX( 1 , NPAR(IP)-1 )
        DPAR(IP) = (PAR2(IP)-PAR1(IP))/FLOAT(NPM)
      ENDDO
C

      IPA = IPSWEEP(1)
      IPB = IPSWEEP(2)
      IPC = IPSWEEP(3)
      IPD = IPSWEEP(4)
C
      NPA = MAX( 1 , NPAR(IPA) )
      NPB = MAX( 1 , NPAR(IPB) )
      NPC = MAX( 1 , NPAR(IPC) )
      NPD = MAX( 1 , NPAR(IPD) )

c      write(*,'(1x,8i4)') (npar(ip)  , ip = 1, iptot)
c      write(*,'(1x,8i4)') (ipsweep(k),  k = 1, iptot)
c      write(*,'(1x,8i4)') ipa, ipb, ipc, ipd
c      write(*,'(1x,8g12.4)') (par1(ip), ip=1,iptot)


      DO 500 KPA = 1, NPA
      DO 400 KPB = 1, NPB
      DO 300 KPC = 1, NPC
      DO 200 KPD = 1, NPD
        DO IP = 1, IPTOT
          PAR(IP) = PAR1(IP)
C
          IF(IP.EQ.IPA) THEN
           PAR(IP) = PAR1(IP) + DPAR(IP)*FLOAT(KPA-1)
          ENDIF
C
          IF(IP.EQ.IPB) THEN
           PAR(IP) = PAR1(IP) + DPAR(IP)*FLOAT(KPB-1)
          ENDIF
C
          IF(IP.EQ.IPC) THEN
           PAR(IP) = PAR1(IP) + DPAR(IP)*FLOAT(KPC-1)
          ENDIF
C
          IF(IP.EQ.IPD) THEN
           PAR(IP) = PAR1(IP) + DPAR(IP)*FLOAT(KPD-1)
          ENDIF
        ENDDO
C
C------ set specified or current parameter values
        VEL  = PAR(IPVEL )
        RPM  = PAR(IPRPM )
        VOLT = PAR(IPVOLT)
        DBET = PAR(IPDBET)
C
        DBE = DBET * PI/180.0
        OMG = RPM  * PI/30.0
C
C------ set initial omega if necessary
        IF(OMG .LE. 0.0) THEN
C------- guess using 80% radius effective pitch angle
         I = MAX( 1 , (8*N)/10 )
         RT = R(I)
         BT = B(I) - CL0(I)/DCLDA(I) + DBE
         BT = MAX( 0.02 , MIN( 0.45*PI , BT ) )
         IF(VEL.EQ.0.0) THEN
          OMG = 1.0
         ELSE
          OMG = VEL/(RT*TAN(BT))
         ENDIF
        ENDIF
C
c        IF(VOLT .EQ. 0.0) THEN
cC------- set voltage to get zero torque
c         QP = 0.
c         CALL VOLTM(OMG,QP, IMOTYPE, PARMOT,NMPAR,
c     &              VM,VM_OMG,VM_QP,
c     &              AM,AM_OMG,AM_QP )
c         VOLT = VM
c        ENDIF
C
C------ Newton iteration on state, to converge on specified quantities
        DO 100 ITER = 1, 25
C
C-------- clear Newton system
          DO K = 1, 4
            DO L = 1, 4
              ASYS(K,L) = 0.
            ENDDO
            RES(K) = 0.
          ENDDO
C
C-------- calculate prop operation for current state
C-        TP(VEL,OMG,DBR) 
C-        QP(VEL,OMG,DBR) 

          CALL TQCALC(N,C,B,R,DR,
     &            VA,VT,CL,CD,STALL,
     &            BLDS,RAD,VEL,OMG,DBE,
     &            RHO,RMU,VSO,
     &            CL0,DCLDA,CLMIN,CLMAX,MCRIT,
     &            CD0,CD2U,CD2L,CLCD0,REREF,REEXP,
     &            TP, TP_VEL, TP_OMG, TP_DBE, TP_C, TP_B,
     &            QP, QP_VEL, QP_OMG, QP_DBE, QP_C, QP_B )
C
C-------- set motor torque 
C-        QM(OMG,VOLT)
          CALL MOTORQ(OMG,VOLT, IMOTYPE, PARMOT,NMPAR,
     &                QM,QM_OMG,QM_VOLT,
     &                AM,AM_OMG,AM_VOLT )

c          write(*,*) qm, qm_omg, qm_volt
c          write(*,*) am, am_omg, am_volt

C
          AMPS = AM
          PELE = AM*VOLT
C
 7000     FORMAT(/ 1X,'*** Ill-posed problem:  Unable to set  ', A / )
C
C-------- initialize Newton equation row index, and torque-match flag
          KEQ = 0
          LQMATCH = .FALSE.
C
          IF(NPAR(IPVEL).GT.0) THEN
C--------- Res = VEL - VEL_spec
           KEQ = KEQ + 1
           RES(KEQ)    = VEL - PAR(IPVEL)
           ASYS(KEQ,1) = 1.0
          ELSEIF(.NOT.LQMATCH) THEN
C--------- Res = QM - QP
           KEQ = KEQ + 1
           RES(KEQ)    = QM      - QP
           ASYS(KEQ,1) =         - QP_VEL
           ASYS(KEQ,2) = QM_OMG  - QP_OMG
           ASYS(KEQ,3) = QM_VOLT
           ASYS(KEQ,4) =         - QP_DBE
           LQMATCH = .TRUE.
c          ELSE
c           WRITE(*,7000) PARNAME(IPVEL)
c           STOP
          ENDIF
C
          IF(NPAR(IPRPM).GT.0) THEN
C--------- Res = OMG - OMG_spec
           KEQ = KEQ + 1
           RES(KEQ)    = OMG - PAR(IPRPM)*PI/30.0
           ASYS(KEQ,2) = 1.0
          ELSEIF(.NOT.LQMATCH) THEN
C--------- Res = QM - QP
           KEQ = KEQ + 1
           RES(KEQ)    = QM      - QP
           ASYS(KEQ,1) =         - QP_VEL
           ASYS(KEQ,2) = QM_OMG  - QP_OMG
           ASYS(KEQ,3) = QM_VOLT
           ASYS(KEQ,4) =         - QP_DBE
           LQMATCH = .TRUE.
c          ELSE
c           WRITE(*,7000) PARNAME(IPRPM)
c           STOP
          ENDIF
C
          IF(NPAR(IPVOLT).GT.0) THEN
C--------- Res = VOLT - VOLT_spec
           KEQ = KEQ + 1
           RES(KEQ)    = VOLT - PAR(IPVOLT)
           ASYS(KEQ,3) = 1.0
          ELSEIF(.NOT.LQMATCH) THEN
C--------- Res = QM - QP
           KEQ = KEQ + 1
           RES(KEQ)    = QM      - QP
           ASYS(KEQ,1) =         - QP_VEL
           ASYS(KEQ,2) = QM_OMG  - QP_OMG
           ASYS(KEQ,3) = QM_VOLT
           ASYS(KEQ,4) =         - QP_DBE
           LQMATCH = .TRUE.
c          ELSE
c           WRITE(*,7000) PARNAME(IPVOLT)
c           STOP
          ENDIF
C
          IF(NPAR(IPDBET).GT.0) THEN
C--------- Res = DBE - DBE_spec
           KEQ = KEQ + 1
           RES(KEQ)    = DBE - PAR(IPDBET)*PI/180.0
           ASYS(KEQ,4) = 1.0
          ELSEIF(.NOT.LQMATCH) THEN
C--------- Res = QM - QP
           KEQ = KEQ + 1
           RES(KEQ)    = QM      - QP
           ASYS(KEQ,1) =         - QP_VEL
           ASYS(KEQ,2) = QM_OMG  - QP_OMG
           ASYS(KEQ,3) = QM_VOLT
           ASYS(KEQ,4) =         - QP_DBE
           LQMATCH = .TRUE.
          ELSEIF(KEQ.LT.4) THEN
C--------- Res = DBE - DBE_spec
           KEQ = KEQ + 1
           RES(KEQ)    = DBE - PAR(IPDBET)*PI/180.0
           ASYS(KEQ,4) = 1.0
c          ELSE
c           WRITE(*,7000) PARNAME(IPDBET)
c           STOP
          ENDIF
C
          IF(NPAR(IPTHRU).GT.0 .AND. KEQ.LT.4) THEN
C--------- Res = TP - THRU
           KEQ = KEQ + 1
           RES(KEQ)    = TP - PAR(IPTHRU)
           ASYS(KEQ,1) = TP_VEL
           ASYS(KEQ,2) = TP_OMG
           ASYS(KEQ,4) = TP_DBE
          ENDIF
C
          IF(NPAR(IPTORQ).GT.0 .AND. KEQ.LT.4) THEN
C--------- Res = QP - TORQ
           KEQ = KEQ + 1
           RES(KEQ)    = QP - PAR(IPTORQ)
           ASYS(KEQ,1) = QP_VEL
           ASYS(KEQ,2) = QP_OMG
           ASYS(KEQ,4) = QP_DBE
          ENDIF
C
          IF(NPAR(IPAMPS).GT.0 .AND. KEQ.LT.4) THEN
C--------- Res = AM - AMPS
           KEQ = KEQ + 1
           RES(KEQ)    = AM - PAR(IPAMPS)
           ASYS(KEQ,2) = AM_OMG
           ASYS(KEQ,3) = AM_VOLT
          ENDIF
C
          IF(NPAR(IPPELE).GT.0 .AND. KEQ.LT.4) THEN
C--------- Res = VOLT*AM - PELE
           KEQ = KEQ + 1
           RES(KEQ)    = VOLT*AM  - PAR(IPPELE)
           ASYS(KEQ,2) = VOLT*AM_OMG
           ASYS(KEQ,3) = VOLT*AM_VOLT + AM
          ENDIF
C
ccc       IF(KEQ .NE. 4) WRITE(*,*) '? KEQ =', KEQ

c         write(*,*)
c         do k = 1, 4
c           write(*,'(1x,4f11.5,e14.4)') (asys(k,l), l=1,4), res(k)
c         enddo

          CALL GAUSSN0(4,4,ASYS,RES,1,KZERO)
C
          IF(KZERO.NE.0) THEN
           WRITE(*,8000) PARNAME(KZERO)
 8000      FORMAT(/' *** Ill-posed problem: Unable to determine  ',A /)
           STOP
          ENDIF
C
          DVEL  = -RES(1)
          DOMG  = -RES(2)
          DVOLT = -RES(3)
          DDBE  = -RES(4)

c         write(*,'(1x,4e12.4)'), (-res(k), k=1, 4)
c         pause


          RLX = 1.0
C
          VTIP = OMG*RAD
          IF(RLX*DVEL  .GT.  0.1*VTIP) RLX =  0.1*VTIP/DVEL
          IF(RLX*DVEL  .LT. -0.1*VTIP) RLX = -0.1*VTIP/DVEL
C
          IF(RLX*DOMG  .GT.  1.0*OMG ) RLX =  1.0*OMG /DOMG
          IF(RLX*DOMG  .LT. -0.5*OMG ) RLX = -0.5*OMG /DOMG
C
          IF(RLX*DVOLT .GT.  2.0     ) RLX =  2.0/DVOLT
          IF(RLX*DVOLT .LT. -2.0     ) RLX = -2.0/DVOLT
C
          IF(RLX*DDBE  .GT.  0.02    ) RLX =  0.02/DDBE 
          IF(RLX*DDBE  .LT. -0.02    ) RLX = -0.02/DDBE 
C

c          ddbet = ddbe*180.0/pi
c           write(*,'(1x,i3,4f12.3,2x,4e12.4,f9.3)') 
c     &     iter, vel, omg, volt, dbet, dvel, domg, dvolt, ddbe,rlx

C-------- convergence check
          DMAX = MAX( ABS(DVEL )/VTIP,
     &                ABS(DOMG )/OMG ,
     &                ABS(DVOLT)     ,
     &                ABS(DDBE )/0.05  )
          IF(DMAX .LT. EPS) GO TO 110
C
C-------- Newton update
          VEL  = VEL  + RLX*DVEL
          OMG  = OMG  + RLX*DOMG
          VOLT = VOLT + RLX*DVOLT
          DBE  = DBE  + RLX*DDBE 
C
          RPM  = OMG *  30.0/PI
          DBET = DBE * 180.0/PI
C
C
 100    CONTINUE
        WRITE(*,'(1X,A,8G12.4)') 
     &       'QPROP: Convergence failed. Res =', (RES(K), K=1, 4), DMAX
C
 110    CONTINUE

c      Q = 1.0 / (Kv*pi/30.0) * (I-Io)
c      I = Io + Q*(Kv*pi/30.0)
c      P = (V-I*R) * (I-Io)
c      eff = P / (I*V)
c      rpm = Kv * (V-I*R)
C
C------ compute thrust-average blade cl and cd
        DTSUM = 0.
        CLAVG = 0.
        CDAVG = 0.
        DO I = 1, N
          WA = VEL + VA(I)
          WT = OMG*R(I) - VT(I)
          WSQ = WA**2 + WT**2
          DTSUM = DTSUM + WSQ*C(I)*DR(I)
          CLAVG = CLAVG + WSQ*C(I)*DR(I)*CL(I)
          CDAVG = CDAVG + WSQ*C(I)*DR(I)*CD(I)
        ENDDO
        CLAVG = CLAVG / DTSUM
        CDAVG = CDAVG / DTSUM
C
C------ print output
        RPM = OMG*30.0/PI
        PPROP = TP*VEL
        POWER = QP*OMG
C
        PINPUT = VOLT*AMPS
C
        IF(POWER .NE. 0.0) THEN
         EFFP = PPROP/POWER
        ELSE
         EFFP = 0.
        ENDIF
C
        IF(PINPUT .NE. 0.0) THEN
         EFFM = POWER/PINPUT
        ELSE
         EFFM = 0.0
        ENDIF
C
        EFF = EFFM*EFFP
C
        IF(ABS(OMG).GT.0.0) THEN
         ADV = VEL/(OMG*RAD)
        ELSE
         ADV = 0.
        ENDIF
C
        IF(OMG .EQ. 0.0) THEN
         WRI = 0.
        ELSE
         WRI = 1.0 / (OMG*RAD)
        ENDIF
C
        CT = TP * WRI**2 * 2.0 / (RHO * PI * RAD**2)
        CP = QP * WRI**2 * 2.0 / (RHO * PI * RAD**3)
        DV = SQRT(VEL**2 + TP * 2.0/(RHO*PI*RAD**2)) - VEL
C
        WRITE(LU,2100) CHARF,
     &      VEL,   RPM,  DBET,   TP,     QP, POWER, 
     &     VOLT,AMPS,  EFFM,  EFFP, ADV, CT, CP, DV, 
     &     EFF, PINPUT, PPROP, CLAVG, CDAVG
 2100   FORMAT(A,
     &     F8.3,  G12.4,  F7.3, G12.4, G12.4, G12.4,
     &     F8.3, F10.4,  F9.4, F9.4, F10.5, G12.4, G12.4, F9.4,
     &     F9.4, G12.4, G12.4, F9.4, G12.4)
C
 200  CONTINUE ! with next KPD
      IF(NPD.GT.1) WRITE(LU,1000)
C
 300  CONTINUE ! with next KPC
      IF(NPC.GT.1) WRITE(LU,1000)
C
 400  CONTINUE ! with next KPB
      IF(NPB.GT.1) WRITE(LU,1000)
C
 500  CONTINUE ! with next KPA
C
      IF(LRDUMP) THEN
C----- dump radial distributions
       WRITE(LU,1100)
       WRITE(LU,1100)
     & ' radius   chord   beta      Cl       Cd       Re    Mach',
     & '     effi     effp    Wa(m/s)     Aswirl      adv_wake'
C                              123456789012123456789012123456789012
       DO I = 1, N
         WA = VEL + VA(I)
         WT = OMG*R(I) - VT(I)
         WSQ = WA**2 + WT**2
         W = SQRT(WSQ)
C
C------- local Mach and Re
         AMA = W/VSO
         IRE = INT( RHO*W*C(I)/RMU + 0.5 )
C
C------- local wake advance ratio, induced and profile efficiencies
         IF(WA.NE.0.0 .AND. WT.NE.0.0) THEN
          ADW = (WA/WT) * (R(I)/RAD)
          EFFI = (VEL/(OMG*R(I))) * (WT/WA)
          EFFP = (CL(I) - CD(I)*WA/WT)
     &         / (CL(I) + CD(I)*WT/WA)
         ELSE
          ADW = 0.
          EFFI = 0.
          EFFP = 0.
         ENDIF
C
         EFFI = MAX( -99.0 , MIN( 99.0 , EFFI ) )
         EFFP = MAX( -99.0 , MIN( 99.0 , EFFP ) )
C
         RU = R(I)
         CU = C(I)
         BU = B(I) * 180.0/PI
C
C------- swirl flow angle in non-rotating frame
         ASWIRL = ATAN2( VT(I) , WA ) * 180.0/PI
C
         WRITE(LU,3100) 
     &               RU,   CU,   BU, CL(I), CD(I),
     &              IRE,  AMA, EFFI, EFFP, WA, ASWIRL, ADW
 3100    FORMAT(1X,F8.4, F8.4, F8.3, F9.4,  F9.5,
     &               I9, F7.3, F9.4, F9.4, G12.4, G12.4, G12.4)
       ENDDO
      ENDIF
C
      STOP
C
 900  CONTINUE
      WRITE(*,9000) FILNAM(1:64), ILINE, LINE
 9000 FORMAT(/' Read error'
     &       /'   in file:  ', A
     &       /'   line',I3,':  ', A)
      STOP
C
 950  CONTINUE
      WRITE(*,9500) FILNAM(1:64), ILINE
 9500 FORMAT(/' Unexpected end-of-file reached'
     &       /'   in file:  ', A
     &       /'   line',I3)
      STOP
C
C
 980  CONTINUE
      WRITE(*,9500) FILNAM(1:64), ILINE
 9800 FORMAT(/' Fewer parameters than required'
     &       /'   in file:  ', A
     &       /'   line',I3)
      STOP
C
      END ! QPROP
         
