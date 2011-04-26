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
	SUBROUTINE M_PROP_ANALYSE
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Existing Motor + Prop Analyse Subroutine '
!--- Loop for all motor and props... ---!

	indx_motor=indx_motor_ary(1)
	do while (indx_motor .le. indx_motor_ary(2))
	indx_prop=indx_prop_ary(1)
	do while (indx_prop .le.indx_prop_ary(2))

!According to the index number, get the motor name for Qprop calculations

	CALL Get_motor_name (indx_motor,motor_name)
	CALL Get_prop_name (indx_prop,prop_name)
	WRITE(*,*)'*****************	',trim(prop_name) ,'	', trim(motor_name), '		**************************************'
!	write(*,500) trim(qprop_infile), trim(motor_name), Speed(wcn), Thrust(wcn), trim(qprop_outfile)

!!! The mass depends on the propeller and the engine used so it cannot be calculated before the loop for now.

	CALL PROP_DATA_FINDER

!The PROP_RADIUS has been set and the optimun size can be computed
	CALL MFRAME

	CALL M_TOTAL_WEIGHT
	Thrust(wcn) = M_TOTAL * GRAV_ACC / NR_MOTOR


!	write(*,*)qprop_in_command
	WRITE(qprop_in_command,500) trim(prop_name), trim(motor_name), Speed(wcn), Thrust(wcn), trim(qprop_outfile) 

	write(*,*) qprop_in_command
!500	Format ('../../BIN/qprop ',A,' ',A )

500	Format ('../../BIN/qprop',' ./DATA/PROPELLER/',A,' ./DATA/MOTOR/',A,' ',F5.2,' - - 0 ',F5.2,' > ',A )
	
	Call system (qprop_in_command)
! Read the results from Qprop out file
	Call qprop_read (Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, &
                                    Qprop_Volts, Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, & 
                                    Qprop_CP, Qprop_DV, Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, &
                                    Qprop_cd_avg, qprop_outfile, err_nr )
!--- Debug Print...
		write (*,*)
		write (*,*) 'Working Cond          :  ', wcn
		write (*,*) 'Motor Name            :  ', motor_name
		write (*,*) 'Prop Name             :  ', prop_name 
		write (*,*) 'MASS                  :  ', M_TOTAL 
		write (*,*) 'PROP Eff              :  ', Qprop_Eff_prop
		write (*,*) 'MOTOR Eff             :  ', Qprop_Eff_mot
		write (*,*) 'Total Eff             :  ', Qprop_Eff_total
		write (*,*) 'Torque                :  ', Qprop_Q
	 	write (*,*) 'Thrust                :  ', Qprop_T
	 	write (*,*) 'Volts                 :  ', Qprop_Volts
	 	write (*,*) 'Amps                  :  ', Qprop_Amps
	 	write (*,*) 'Electrical Power      :  ', Qprop_P_elec
	 	write (*,*)

		CALL M_MAX_FLIGHT_TIME
		CALL TW_RATIO_ESTIMATOR

		IF ( MIN_TW_RATIO .Le. TW_RATIO) THEN
			CALL CREATE_OUTPUT_TABLE(prop_name, motor_name, BATT_SPEC_NRG * M_BATT, M_TOTAL, Qprop_T, TOTAL_FLYING_POWER, &
				TW_RATIO, MAX_FLIGHT_TIME)
		END IF
!--- Just after having all the coeffs, Simulation needs to be called in Mexec.f90, to calculte the mission...

	indx_prop=indx_prop+1
	end do ! indx_prop loop
	indx_motor=indx_motor+1
	end do ! indx_motor loop



	END SUBROUTINE M_PROP_ANALYSE















	SUBROUTINE M_MAX_FLIGHT_TIME
	USE MCOMMON
	IMPLICIT NONE

	REAL :: NRG, HOVER_POWER

	INTEGER :: MAX_FLIGHT_TIME_MIN, MAX_FLIGHT_TIME_HOUR

	NRG = BATT_SPEC_NRG * M_BATT


!--- for the engines only, payload and autopilot power are added after
	HOVER_POWER = Qprop_P_elec * Nr_motor

	write (*,*)
	write (*,*) 'the total energy inboard is :  ', NRG, 'Wh'		! debug
	write (*,*) 'the power required to hover is :  ', HOVER_POWER, 'W'		! debug

	CALL CONTROLLER_EFFICIENCY_ESTIMATOR

	write (*,*) 'the controllers efficiencies are :  ', CONTROLLER_ESTIMATED_EFFICIENCY		! debug

	TOTAL_FLYING_POWER = HOVER_POWER / CONTROLLER_ESTIMATED_EFFICIENCY + AVIONICS_POWER + PAYLOAD_POWER

	MAX_FLIGHT_TIME = NRG / TOTAL_FLYING_POWER * 60	! flight time in minutes

	MAX_FLIGHT_TIME_HOUR= MAX_FLIGHT_TIME/ 60

	MAX_FLIGHT_TIME_MIN= modulo(MAX_FLIGHT_TIME,60)

	write (*,*) 'the total power needed is :  ', TOTAL_FLYING_POWER, 'W'		! debug
	write (*,*) 'the maximal flight time is  :  ', MAX_FLIGHT_TIME_HOUR,'h',MAX_FLIGHT_TIME_MIN		! debug
	write (*,*)
	

	END SUBROUTINE M_MAX_FLIGHT_TIME



	SUBROUTINE CONTROLLER_EFFICIENCY_ESTIMATOR
	USE MCOMMON
	IMPLICIT NONE

	REAL MIN_EFF, MAX_EFF

!--- This is a really simple model that needs to be improved
	
	MIN_EFF = 0.5
	MAX_EFF = 0.9

	CONTROLLER_ESTIMATED_EFFICIENCY = MIN_EFF  + (Qprop_Volts / BATT_MAX_VOLT) * (MAX_EFF - MIN_EFF)


	END SUBROUTINE CONTROLLER_EFFICIENCY_ESTIMATOR

















SUBROUTINE TW_RATIO_ESTIMATOR
	USE MCOMMON
	IMPLICIT NONE

!	write(*,*)qprop_in_command
	WRITE(qprop_in_command,500) trim(prop_name), trim(motor_name), Speed(wcn), BATT_MAX_VOLT, trim(qprop_outfile) 

!	write(*,*) qprop_in_command
!500	Format ('../../BIN/qprop ',A,' ',A )

500	Format ('../../BIN/qprop',' ./DATA/PROPELLER/',A,' ./DATA/MOTOR/',A,' ',F5.2,' - ',F5.2,'0 > ',A )
	
	Call system (qprop_in_command)
! Read the results from Qprop out file
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
!	 	write (*,*) 'Electrical Power      :  ', Qprop_P_elec
!	 	write (*,*)

	TW_RATIO = NR_MOTOR *  Qprop_T / (M_TOTAL * GRAV_ACC )

	write (*,*) 'the thrust to weight ratio is :  ', TW_RATIO
	write (*,*)

	END SUBROUTINE TW_RATIO_ESTIMATOR


