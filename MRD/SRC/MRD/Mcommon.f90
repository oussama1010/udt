!-----------------------------------------------------------------------------|
!    Consists a part of MRD Program - Multi Rotor Vehicle Design, see MRD.f90 |
!    Copyright (C) 2011  Murat BRONZ                                          |
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
	MODULE MCOMMON
	IMPLICIT NONE

	REAL :: VERSION

	CHARACTER(LEN=120) :: LINE
	CHARACTER(LEN=40) :: COMMAND_LINE_ARG, CASE_FILE_NAME
	INTEGER :: K, KI, KBLANK

	CHARACTER(LEN=30) :: COMMAND
	CHARACTER(LEN=90) :: LEFT_ARGS
	REAL :: R1,R2,R3
	
	REAL ::		RHO,		&
			MU, 		&
			VSOUND


	INTEGER :: 	NR_MOTOR,	&
			NR_PROP,	&
			NR_BLADE,	&
			RUN_MODE,	&
			FRAME_TYPE,	&
			FRAME_MAT,	&
			FRAME_SHAPE




	REAL :: TIP_CLRNC


	REAL :: M_FRAME,	&
		M_FRAME_FIX,	&
		M_PROP,		&
		M_BATT,		&
		M_MOTOR,	&
		M_TOTAL

!--- New addition from MURAT
	REAL :: Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, Qprop_Volts, &
                Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, Qprop_CP, Qprop_DV,&
                Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, Qprop_cd_avg 

	CHARACTER(len=25) :: propeller_candidate, qprop_outfile, Prop_name, Motor_name, Airfoil_name
	CHARACTER(len=50) :: qprop_infile
	CHARACTER(len=120) :: qprop_in_command
	CHARACTER(len=27) :: dir_name

	INTEGER :: wcn, err_nr

	INTEGER :: indx_airfoil, indx_motor, indx_prop, indx_motor_ary(3),&
		indx_airfoil_ary(3), indx_prop_ary(3)

	REAL :: Speed(3), Thrust(3)

	INTEGER :: n_prop, n_motor !these are the motor and prop number in the directory... we should change the name I guess...

!--- New addition from Charles
	REAL :: PROP_BLADE_SIGMA, PROP_HUB_COEFF, PROP_RADIUS, FRAME_SPAN

	REAL :: M_PAYLOAD, M_AUTOP, M_MISC, BATT_SPEC_NRG

	REAL :: GRAV_ACC
	
	REAL :: BATT_MAX_VOLT, CONTROLLER_ESTIMATED_EFFICIENCY, AVIONICS_POWER, PAYLOAD_POWER, TW_RATIO, TOTAL_FLYING_POWER

	INTEGER :: MAX_FLIGHT_TIME

	Character(len=30),dimension(2,1000) :: table1
	Real,dimension(8,1000) :: table2
	Integer,dimension(2,1000) :: table3

	Integer :: n, MISSION_SCORE, FTIME_COEFF, SIZE_COEFF, TW_COEFF

	REAL :: M_BATT_MIN, M_BATT_MAX, M_BATT_DELTA, MIN_TW_RATIO

	END MODULE MCOMMON
