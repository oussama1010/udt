!-----------------------------------------------------------------------------|
!    Consists a part of MRD Program - Multi Rotor Vehicle Design, see MRD.f90 |
!    Copyright (C) 2011  Murat BRONZ & Charles PLACHOT                        |
!                                                                             |
!    This program is free software; you can redistribute it and/or modify     |
!    it under the terms of the GNU General Public License as published by     |
!    the Free Software Foundation; either version 2 of the License, or        |
!    (at your option) any later version.                                      |
!                                                                             |
!    This program is distributed in the hope that it will be useful,          |
!    but WITHOUT ANY WARRANTY; without even the implied warranty of           |
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            |
!    GNU General Public License for more details.                             |
!                                                                             |
!    You should have received a copy of the GNU General Public License along  |
!    with this program; if not, write to the Free Software Foundation, Inc.,  |
!    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.              |
!-----------------------------------------------------------------------------|
	SUBROUTINE M_SIMUL
	USE MCOMMON
	IMPLICIT NONE
! this subroutine runs a Qprop simulation for the current aircraft configuration
! the total weight is calculated according to the different components
!A simulation is then launched and stored inside the output.dat file if the results meet the mission constraints
 
	WRITE(*,*)'QProp Simulation'

	prop_cut = INDEX(prop_name,' ')

!!! The mass depends on the propeller and the engine used so it cannot be calculated before the loop for now.
	CALL PROP_DATA_FINDER

!---The PROP_RADIUS is known and the optimun size can be computed
	CALL MFRAME

	IF (CANCEL_SIMUL .eq. 1) THEN
		WRITE(*,*)'the propeller is not suitable'
		GOTO 1000
	ENDIF

!---Knowing the frame size the total weight can be calculated and gives the needed thrust of each motor 
	CALL M_TOTAL_WEIGHT
	
	CALL TRANSLATION_BANK_ANGLE_ESTIMATOR

	Thrust(wcn) = M_TOTAL * GRAV_ACC / (NR_MOTOR * COS(PHI))
	
	IF (TRANSLATION_SPEED .ne. 0) THEN	
		Beta = 3.14 / 2 - PHI
		Speed(wcn) = 0.06 + TRANSLATION_SPEED
	ELSE 
		Beta = 0
	END IF


!---The command to call Qprop is created and launched
	IF (RUN_MODE .EQ. 2) THEN 
		WRITE(qprop_in_command,500) prop_name(1:prop_cut-1), trim(motor_name), Speed(wcn), Beta, Thrust(wcn), trim(qprop_outfile) 
	ELSE
		WRITE(qprop_in_command,550) prop_name(1:prop_cut-1), trim(motor_name), Speed(wcn), Beta, Thrust(wcn), trim(qprop_outfile) 
	END IF

500		Format ('../../BIN/qprop',' ./RESULTS/PROPELLER/',A,' ./DATA/MOTOR/',A,' ',F5.2,' - - ',F5.2,' ',F5.2,' > ',A )
550		Format ('../../BIN/qprop',' ./DATA/PROPELLER/',A,' ./DATA/MOTOR/',A,' ',F5.2,' - - ',F5.2,' ',F5.2,' > ',A )

!		write(*,*) qprop_in_command


	
		Call system (qprop_in_command)


!---Read the results from Qprop out file
		Call qprop_read (Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, &
                                    Qprop_Volts, Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, & 
                                    Qprop_CP, Qprop_DV, Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, &
                                    Qprop_cd_avg, qprop_outfile, err_nr )



!--- Debug Print...
!		write (*,*)
!		write (*,*) 'Working Cond          :  ', wcn
!		write (*,*) 'Motor Name            :  ', motor_name
!		write (*,*) 'Prop Name             :  ', prop_name 
!		write (*,*) 'MASS                  :  ', M_TOTAL 
!		write (*,*) 'PROP Eff              :  ', Qprop_Eff_prop
!		write (*,*) 'MOTOR Eff             :  ', Qprop_Eff_mot
!		write (*,*) 'Total Eff             :  ', Qprop_Eff_total
!		write (*,*) 'Torque                :  ', Qprop_Q
!	 	write (*,*) 'Thrust                :  ', Qprop_T
!	 	write (*,*) 'Volts                 :  ', Qprop_Volts
!	 	write (*,*) 'Amps                  :  ', Qprop_Amps
!	 	write (*,*) 'Electrical Power      :  ', Qprop_P_elec
!	 	write (*,*)
!	 	write (*,*) 'RPM      :  ', Qprop_rpm
	VOLTS = Qprop_Volts
	AMPS =  Qprop_Amps

	P_ELEC = Qprop_P_elec


!---Calls the subroutine finding the maximal flight time and range
	CALL M_MAX_FLIGHT_TIME
	CALL M_MAX_RANGE

!---Calls the subroutine finding the maximal thrust/weight ratio
	CALL TW_RATIO_ESTIMATOR

	CALL YAW_ANGULAR_ACCELERATION_ESTIMATOR


	CALL FILL_GNUPLOT_DATA

! Only the configurations meeting the mission constraints are stored
		IF ( MIN_TW_RATIO .Le. TW_RATIO .AND. AMPS .LE. MAX_STEADY_CURRENT .AND. MAX_OUTPUT_CURRENT .LE. &
		MAX_BURST_CURRENT ) THEN
			CALL CREATE_OUTPUT_TABLE
		ELSE
			WRITE(*,*)MIN_TW_RATIO, TW_RATIO
			WRITE(*,*)AMPS,  MAX_STEADY_CURRENT
			WRITE(*,*)MAX_OUTPUT_CURRENT, MAX_BURST_CURRENT
			WRITE(*,*)'Configuration not suitable'
			WRITE(*,*)''
		END IF

1000	CONTINUE

	END SUBROUTINE M_SIMUL



!##########################################################################################################################
	SUBROUTINE M_SIMPLIFIED_SIMUL
	USE MCOMMON
	IMPLICIT NONE
! this subroutine uses a mathematical model of the current aircraft configuration
! the total weight is calculated according to the different components
!the result is stored inside the output.dat file if the mission constraints are met
 
	WRITE(*,*)'Simplified Simulation'


!---The PROP_RADIUS is known and the optimun size can be computed
	CALL MFRAME

	IF (CANCEL_SIMUL .eq. 1) THEN
		WRITE(*,*)'the propeller is not suitable'
		GOTO 1000
	ENDIF

!---Knowing the frame size the total weight can be calculated and gives the needed thrust of each motor 
	CALL M_TOTAL_WEIGHT
	
	CALL TRANSLATION_BANK_ANGLE_ESTIMATOR

	Thrust(wcn) = M_TOTAL * GRAV_ACC / (NR_MOTOR * COS(PHI))

!---Knowing the thrust need needed we can find the RPM which gives the torque
	CALL SOLVE_SECOND_ORDER(K2_THRUST, K1_THRUST , K0_THRUST - Thrust(wcn), RPM)

	TORQUE = K0_TORQUE + K1_TORQUE * RPM + K2_TORQUE * RPM**2

!---We can now calculate the power consumption using the motor parameters.
	P_MECA = TORQUE * RPM *3.14*2/60

	AMPS = TORQUE * KV_MOTOR*3.14*2/60 + I0_MOTOR

	VOLTS = RPM / (KV_MOTOR) + R_MOTOR * AMPS

	P_ELEC = VOLTS * AMPS


!--- Debug Print...
!		write (*,*)
!		write (*,*) 'Working Cond          :  ', wcn
!		write (*,*) 'Motor Name            :  ', motor_name
!		write (*,*) 'Prop Name             :  ', prop_name 
!		write (*,*) 'MASS                  :  ', M_TOTAL 
!		write (*,*) 'PROP Eff              :  ', Qprop_Eff_prop
!		write (*,*) 'MOTOR Eff             :  ', Qprop_Eff_mot
!		write (*,*) 'Total Eff             :  ', Qprop_Eff_total
!		write (*,*) 'Torque                :  ', TORQUE
!	 	write (*,*) 'Thrust                :  ', Thrust(wcn)
!	 	write (*,*) 'Kv                :  ', Kv_MOTOR
!	 	write (*,*) 'RPM                   :  ', RPM
!	 	write (*,*) 'Volts                 :  ', VOLTS
!	 	write (*,*) 'Amps                  :  ', AMPS
!	 	write (*,*) 'I0                  :  ', I0_MOTOR
!	 	write (*,*) 'Mecanical Power      :  ', P_MECA
!	 	write (*,*) 'Electrical Power      :  ', P_ELEC
!	 	write (*,*)


!---Calls the subroutine finding the maximal flight time and range
	CALL M_MAX_FLIGHT_TIME
	
!--- The range cannot be accurately computed in this mode as the propeller model describes only the static condition
	MAX_RANGE = 0

!---Calls the subroutine finding the maximal thrust/weight ratio
	CALL SIMPLIFIED_TW_RATIO_ESTIMATOR

	CALL SIMPLIFIED_YAW_ANGULAR_ACCELERATION_ESTIMATOR


	CALL FILL_GNUPLOT_DATA

! Only the configurations meeting the mission constraints are stored
		IF ( MIN_TW_RATIO .Le. TW_RATIO .AND. AMPS .LE. MAX_STEADY_CURRENT .AND. MAX_OUTPUT_CURRENT .LE. &
		MAX_BURST_CURRENT ) THEN
			CALL CREATE_OUTPUT_TABLE
		ELSE
			WRITE(*,*)''
			WRITE(*,*)'Configuration not suitable'
			WRITE(*,*)''
		END IF

1000	CONTINUE

	END SUBROUTINE M_SIMPLIFIED_SIMUL



!##########################################################################################################################
	SUBROUTINE M_MAX_FLIGHT_TIME
	USE MCOMMON
	IMPLICIT NONE



	REAL :: NRG, HOVER_POWER

	INTEGER :: MAX_FLIGHT_TIME_MIN, MAX_FLIGHT_TIME_HOUR

	WRITE(*,*)'	Max flight time'

	NRG = BATT_SPEC_NRG * M_BATT


!--- for the engines only, payload and autopilot power are added after
	HOVER_POWER = P_ELEC * Nr_motor

!	write (*,*)
!	write (*,*) 'the total energy inboard is :  ', NRG, 'Wh'		! debug
!	write (*,*) 'the power required to hover is :  ', HOVER_POWER, 'W'		! debug

	CALL CONTROLLER_EFFICIENCY_ESTIMATOR

!	write (*,*) 'the controllers efficiencies are :  ', CONTROLLER_ESTIMATED_EFFICIENCY		! debug

	TOTAL_FLYING_POWER = HOVER_POWER / CONTROLLER_ESTIMATED_EFFICIENCY + AVIONICS_POWER + PAYLOAD_POWER

	MAX_FLIGHT_TIME_FLOAT =  NRG / TOTAL_FLYING_POWER * 60	! flight time in minutes

	MAX_FLIGHT_TIME = MAX_FLIGHT_TIME_FLOAT

!	MAX_FLIGHT_TIME_HOUR= MAX_FLIGHT_TIME/ 60

!	MAX_FLIGHT_TIME_MIN= modulo(MAX_FLIGHT_TIME,60)

!	write (*,*) 'the total power needed is :  ', TOTAL_FLYING_POWER, 'W'		! debug
!	write (*,*) 'the maximal flight time is  :  ', MAX_FLIGHT_TIME,'min'		! debug
!	write (*,*)
	

	END SUBROUTINE M_MAX_FLIGHT_TIME



!##########################################################################################################################
	SUBROUTINE CONTROLLER_EFFICIENCY_ESTIMATOR
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)'		Controller effieciency estimator'

!--- This is a really simple model that needs to be improved
	
!	CNTRLR_MIN_EFF = 0.5
!	CNTRLR_MAX_EFF = 0.9

	CONTROLLER_ESTIMATED_EFFICIENCY = CNTRLR_MIN_EFF  + (VOLTS / BATT_MAX_VOLT) * (CNTRLR_MAX_EFF - CNTRLR_MIN_EFF)


	END SUBROUTINE CONTROLLER_EFFICIENCY_ESTIMATOR



!##########################################################################################################################
	SUBROUTINE TW_RATIO_ESTIMATOR
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)'	Thrust/weight estimator'

	!REAL :: INPUT_VOLTS

	Speed(wcn) = 0.1
	!INPUT_VOLTS = BATT_MAX_VOLT

!--- The max thrust of a motor is computed using Qprop 
	IF (RUN_MODE .EQ. 2) THEN 
		WRITE(qprop_in_command,500)prop_name(1:prop_cut-1), trim(motor_name), Speed(wcn), BATT_MAX_VOLT, trim(qprop_outfile) 
	ELSE
		WRITE(qprop_in_command,550) prop_name(1:prop_cut-1), trim(motor_name), Speed(wcn), BATT_MAX_VOLT, trim(qprop_outfile) 
	END IF


500	Format ('../../BIN/qprop',' ./RESULTS/PROPELLER/',A,' ./DATA/MOTOR/',A,' ',F5.2,' - ',F5.2,'0 > ',A )
550	Format ('../../BIN/qprop',' ./DATA/PROPELLER/',A,' ./DATA/MOTOR/',A,' ',F5.2,' - ',F5.2,'0 > ',A )
	
	Call system (qprop_in_command)

!---Read the results from Qprop out file
	Call qprop_read (Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, &
                                    Qprop_Volts, Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, & 
                                    Qprop_CP, Qprop_DV, Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, &
                                    Qprop_cd_avg, qprop_outfile, err_nr )

!---The thrust weight ratio is then calculated
	TW_RATIO = NR_MOTOR *  Qprop_T / (M_TOTAL * GRAV_ACC )

	MAX_OUTPUT_CURRENT = Qprop_Amps

!	write (*,*) 'the thrust to weight ratio is :  ', TW_RATIO

	END SUBROUTINE TW_RATIO_ESTIMATOR



!##########################################################################################################################
	SUBROUTINE SIMPLIFIED_TW_RATIO_ESTIMATOR
	USE MCOMMON
	IMPLICIT NONE

	REAL :: Q, I, V, ERROR, PREV_ERROR, DELTA_RPM

	WRITE(*,*)'	Simplified  Thrust/weight estimator'


	DELTA_RPM = 1000
	PREV_ERROR = 1


	DO k= 1, 100

		Q =K0_TORQUE + K1_TORQUE * RPM + K2_TORQUE * RPM**2

		I = Q * KV_MOTOR*3.14*2/60 + I0_MOTOR

		V = RPM / (KV_MOTOR) + R_MOTOR * I

		ERROR = BATT_MAX_VOLT - V

		IF (ABS(ERROR) .Le. 0.001) THEN
			EXIT
		ELSE IF ( ERROR * PREV_ERROR .Le. 0) THEN
			DELTA_RPM = DELTA_RPM / 2
		END IF

		RPM = RPM + SIGN( DELTA_RPM , ERROR)
		PREV_ERROR = ERROR

	END DO
	
	TW_RATIO = NR_MOTOR *(K0_THRUST+ K1_THRUST* RPM +  K2_THRUST* RPM**2)  / (M_TOTAL * GRAV_ACC )

	MAX_OUTPUT_CURRENT = I

!	write (*,*) 'the thrust to weight ratio is :  ', TW_RATIO	 !debug

	END SUBROUTINE SIMPLIFIED_TW_RATIO_ESTIMATOR



!##########################################################################################################################
	SUBROUTINE TRANSLATION_BANK_ANGLE_ESTIMATOR
	USE MCOMMON
	IMPLICIT NONE

	REAL :: SCDRAG0, SCDRAG_MAX, PHI_MAX, THRUSTtemp, ERROR , PREV_ERROR, DELTA_PHI, F, &
		DRAG, SCDRAG

	write (*,*) '	bank angle estimator'


	SCDRAG0 = 0.01
	SCDRAG_MAX = 0.02
	PHI_MAX = 0.6
	DELTA_PHI = 0.4
	PREV_ERROR = 1
	SCDRAG=0

!--- If Phi is not set to 0 the initial value is the result for the previous config so the convergeance is quicker
!	PHI = 0

	DO k=1, 100

!---need to be improved to take into account the size and shape of the frame
		SCDRAG = SCDRAG0 + (SCDRAG_MAX - SCDRAG0)*ABS(PHI/PHI_MAX)

		DRAG = 0.5 * SCDRAG * TRANSLATION_SPEED**2

		THRUSTtemp = M_TOTAL * GRAV_ACC / (NR_MOTOR * cos(PHI))

		F = THRUSTtemp * sin (PHI)

		ERROR = DRAG - F

		IF (ABS(ERROR) .Le. 0.001) THEN
			EXIT
		ELSE IF ( ERROR * PREV_ERROR .Le. 0) THEN
			DELTA_PHI = DELTA_PHI / 2
		END IF

		PHI = PHI + SIGN( DELTA_PHI , ERROR)
		PREV_ERROR = ERROR
 		
	END DO
	


	END SUBROUTINE TRANSLATION_BANK_ANGLE_ESTIMATOR



!##########################################################################################################################
	SUBROUTINE M_MAX_RANGE
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)'	Max range estimator'

	MAX_RANGE = MAX_FLIGHT_TIME * 60 * TRANSLATION_SPEED


	END SUBROUTINE M_MAX_RANGE



!##########################################################################################################################
	SUBROUTINE YAW_ANGULAR_ACCELERATION_ESTIMATOR
	USE MCOMMON
	IMPLICIT NONE

	REAL :: TOTAL_TORQUE, COUNTER_TORQUE
	
	WRITE(*,*)'	yaw angular acceleration estimator'

	CALL MOMENT_OF_INERTIA


!--- Computing the torque making the UAV turn
	IF (RUN_MODE .EQ. 2) THEN 
		WRITE(qprop_in_command,500) prop_name(1:prop_cut-1), trim(motor_name), 0.06, Thrust(wcn) * 1.6, trim(qprop_outfile) 
	ELSE
		WRITE(qprop_in_command,550) prop_name(1:prop_cut-1), trim(motor_name), 0.06, Thrust(wcn) * 1.6, trim(qprop_outfile) 
	END IF

500	Format ('../../BIN/qprop',' ./RESULTS/PROPELLER/',A,' ./DATA/MOTOR/',A,' ',F5.2,' - - 0 ',F5.2,' > ',A )
550	Format ('../../BIN/qprop',' ./DATA/PROPELLER/',A,' ./DATA/MOTOR/',A,' ',F5.2,' - - 0 ',F5.2,' > ',A )
	
	Call system (qprop_in_command)

!---Read the results from Qprop out file
	Call qprop_read (Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, &
                         Qprop_Volts, Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, & 
                         Qprop_CP, Qprop_DV, Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, &
                         Qprop_cd_avg, qprop_outfile, err_nr )

	TOTAL_TORQUE = 2 * Qprop_Q

!--- Computing the remaining torque on the 2 other engines (for now they are set to give 20% of the total lift to keep sufficient control)
	IF (RUN_MODE .EQ. 2) THEN 
		WRITE(qprop_in_command,500) prop_name(1:prop_cut-1), trim(motor_name), 0.06, Thrust(wcn) * 0.4, trim(qprop_outfile)  
	ELSE
		WRITE(qprop_in_command,550) prop_name(1:prop_cut-1), trim(motor_name), 0.06, Thrust(wcn) * 0.4, trim(qprop_outfile)  
	END IF
	

	Call system (qprop_in_command)

!---Read the results from Qprop out file
	Call qprop_read (Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, &
                         Qprop_Volts, Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, & 
                         Qprop_CP, Qprop_DV, Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, &
                         Qprop_cd_avg, qprop_outfile, err_nr )

	COUNTER_TORQUE = 2 * Qprop_Q

	TOTAL_TORQUE = TOTAL_TORQUE - COUNTER_TORQUE

	YAW_ANGULAR_ACCELERATION = TOTAL_TORQUE / I_YAW_TOTAL


	END SUBROUTINE YAW_ANGULAR_ACCELERATION_ESTIMATOR



!##########################################################################################################################
	SUBROUTINE SIMPLIFIED_YAW_ANGULAR_ACCELERATION_ESTIMATOR
	USE MCOMMON
	IMPLICIT NONE

	REAL :: Q, TOTAL_TORQUE, COUNTER_TORQUE
	
	WRITE(*,*)'	Simplified yaw angular acceleration estimator'

	CALL MOMENT_OF_INERTIA

	CALL SOLVE_SECOND_ORDER(K2_THRUST, K1_THRUST , K0_THRUST - 1.6 * Thrust(wcn), RPM)

	Q =   K0_TORQUE + K1_TORQUE * RPM + K2_TORQUE * RPM**2

	TOTAL_TORQUE = 2 * Q

!--- Computing the remaining torque on the 2 other engines (for now they are set to give 20% of the total lift to keep sufficient control)
	CALL SOLVE_SECOND_ORDER(K2_THRUST, K1_THRUST , K0_THRUST - 0.4 * Thrust(wcn), RPM)

	Q =   K0_TORQUE + K1_TORQUE * RPM + K2_TORQUE * RPM**2

	COUNTER_TORQUE = 2 * Qprop_Q

	TOTAL_TORQUE = TOTAL_TORQUE - COUNTER_TORQUE

	YAW_ANGULAR_ACCELERATION = TOTAL_TORQUE / I_YAW_TOTAL


	END SUBROUTINE SIMPLIFIED_YAW_ANGULAR_ACCELERATION_ESTIMATOR

!##################################################################################################################################
	SUBROUTINE SOLVE_SECOND_ORDER(aa,bb,cc,r)
	IMPLICIT NONE

	REAL,intent(in) :: aa,bb,cc
	INTEGER,intent(out) :: r
	REAL ::  DELTA


	DELTA = bb**2 - 4 * aa *cc

	r = (SQRT( DELTA ) - bb) / (2* aa)

	END SUBROUTINE SOLVE_SECOND_ORDER
