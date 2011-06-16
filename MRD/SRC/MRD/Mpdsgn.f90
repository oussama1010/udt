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
	SUBROUTINE M_PROP_DESIGN
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Prop design Subroutine '

	TRANSLATION_SPEED = 0
	Speed(wcn) = 0.06

	indx_airfoil=indx_airfoil_ary(1)
	do while (indx_airfoil .le. indx_airfoil_ary(2))
	NR_BLADE = NR_BLADE_MIN
	do while (NR_BLADE .le. NR_BLADE_MAX)
	PROP_RADIUS = PROP_RADIUS_MIN
	do while (PROP_RADIUS .le. PROP_RADIUS_MAX)
	RPM = RPM_MIN
	do while (RPM .le. RPM_MAX)
	designThrust = Thrust_MIN
	do while (designThrust .le. Thrust_MAX)

		CALL MFRAME

		CALL M_TOTAL_WEIGHT

		 Thrust = M_TOTAL * GRAV_ACC / NR_MOTOR

		CALL GENERATE_PROPELLER

		prop_cut = INDEX(prop_name,' ')

		CALL PROP_DATA_FINDER

		WRITE(*,*)'Aspect ratio :', BLADE_ASPECT_RATIO
		IF (BLADE_ASPECT_RATIO .Le. 3)  THEN
			Call system ('rm ./RESULTS/PROPELLER/'//prop_name(1:prop_cut-1))
		ELSE 
			indx_motor=indx_motor_ary(1)
			do while (indx_motor .le. indx_motor_ary(2))
			M_BATT = M_BATT_MIN 
			do while (M_BATT .le. M_BATT_MAX)

				!CALL Get_motor_name (indx_motor,motor_name)
				CALL Get_motor_specs (indx_motor,motor_name, R_MOTOR, I0_MOTOR, KV_MOTOR,M_MOTOR, MAX_POW_MOTOR)
			WRITE(*,*)'*****************	',prop_name(1:prop_cut-1) ,'	', trim(motor_name),'	**********'
				CALL M_SIMUL	
			M_BATT = M_BATT + M_BATT_DELTA
			end do ! M_BATT loop
			indx_motor=indx_motor+1
			end do ! indx_motor loop
		END IF		
	
	designThrust = designThrust + Thrust_DELTA
	end do ! designThrust loop
	RPM = RPM + RPM_DELTA
	end do ! RPM loop	
	PROP_RADIUS = PROP_RADIUS + PROP_RADIUS_DELTA
	end do ! PROP_RADIUS loop	
	NR_BLADE = NR_BLADE + DELTA_NR_BLADE
	end do ! nr_blade loop
	indx_airfoil=indx_airfoil+1
	end do ! indx_airfoil loop

	END SUBROUTINE M_PROP_DESIGN

!################################################################################################################################
	SUBROUTINE GENERATE_PROPELLER
	USE MCOMMON
	IMPLICIT NONE

		CALL Get_airfoil_spec(indx_airfoil, Airfoil_name,CL0, CLA, CLmin, CLmax, CD0, CD2u, CD2l, CLCD0, REref, REexp)


		IF (RPM .le. 9999) THEN
			WRITE(propeller_candidate,600)trim(Airfoil_name),NR_BLADE,PROP_RADIUS,RPM,designThrust
		ELSE
			WRITE(propeller_candidate,650)trim(Airfoil_name),NR_BLADE,PROP_RADIUS,RPM,designThrust
		END IF

600 	Format (A,'-B',I1,'-R',F3.2,'-RPM',I4,'-T',F3.1)
650 	Format (A,'-B',I1,'-R',F3.2,'-RPM',I5,'-T',F3.1)

		Qmil_RPM = RPM

		CALL Qmil_prop_write(propeller_candidate,NR_BLADE,30, CL0, CLA, CLmin, CLmax, CD0, CD2u, CD2l, CLCD0, &
				REref, REexp, 0, 0.5, 1.0, 0.6, 0.45, 0.4, 0.05*PROP_RADIUS, PROP_RADIUS, 0.1, Qmil_RPM, Thrust(wcn), 0, 0, 0.2)
	

		WRITE(qmil_in_command,500) trim(propeller_candidate),trim(propeller_candidate)

		write(*,*) qmil_in_command

500		Format ('../../BIN/qmil ',A,' ./RESULTS/PROPELLER/',A,' >null')

		prop_name = trim(propeller_candidate)


		Call system (qmil_in_command)

		WRITE(*,*)'Propeller candidate',propeller_candidate 
		Call system ('rm '//propeller_candidate)

	END SUBROUTINE GENERATE_PROPELLER
