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
	


SUBROUTINE M_TOTAL_WEIGHT
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)'	Total Weight Subroutine ' 

!	CALL ENGINE_WEIGHT

!	M_MOTOR = 0.02

!---- Summing all the mass...

	M_total = M_BATT + M_prop*Nr_prop + M_motor*Nr_motor + M_FRAME + M_PAYLOAD + M_AUTOP + M_MISC

!	WRITE(*,*)' M_BATT : ', M_BATT		!debug
!	WRITE(*,*)' M_prop: ', M_prop		!debug
!	WRITE(*,*)' M_motor: ', M_motor	!debug
!	WRITE(*,*)' M_FRAME: ', M_FRAME		!debug
!	WRITE(*,*)' M_AUTOP: ', M_AUTOP		!debug
!	WRITE(*,*)' Total quad Weight is: ', M_total		!debug


	END SUBROUTINE M_TOTAL_WEIGHT


!---retrieves propeller data from the data file...!
	SUBROUTINE PROP_DATA_FINDER	
	USE MCOMMON
	IMPLICIT NONE
	integer ::  status, i, j
	REAL :: radius, chord, radius1, chord1, S, Rfac, Cfac, Bfac

	WRITE(*,*)'	Propeller data finder' 

	radius = 0
	chord = 0
	S = 0

	IF (RUN_MODE .EQ. 2) THEN 
		OPEN(20,file='RESULTS/PROPELLER/'//prop_name(1:prop_cut-1), status='old', iostat=status)
	ELSE
		OPEN(20,file='DATA/PROPELLER/'//prop_name(1:prop_cut-1), status='old', iostat=status)
	END IF

	DO i=1,50
		READ(20,4000,iostat=status)LINE
		IF(status .eq. -1) THEN
			CLOSE(20)
			exit
		END IF


!--- Find the number of blades...
		IF ( LINE( (INDEX(LINE,'!')+2) : (INDEX(LINE,'!')+9) ) .Eq. 'Nblades') THEN
			READ(LINE,*)NR_BLADE
			!WRITE(*,*)'The selected propeller has', NR_BLADE, ' blades'	!debug
		END IF

!--- Find the unit coefficients...
		IF ( LINE( (INDEX(LINE,'!')+3) : (INDEX(LINE,'!')+7) ) .Eq. 'Rfac') THEN
			READ(LINE,*)Rfac, Cfac, Bfac
!			WRITE(*,*)'Rfac, Cfac, Bfac', Rfac, Cfac, Bfac	!debug
		END IF

!--- Find the geometry of the blade and calculate its surface...
		IF ( LINE( 1 : 1) .Eq. '#') THEN
			DO j=1,15
				READ(20,*,iostat=status) radius1, chord1

				radius1 = radius1 * Rfac
				chord1 = chord1 * Cfac

				IF(status.eq.-1) THEN
					GOTO 5000
				END IF

				S=(radius1-radius)*(chord1+chord)*0.5 + S

				radius = radius1
				chord = chord1
			END DO			
		END IF
	END DO

4000	FORMAT(A120)

5000	CONTINUE

	IF (RUN_MODE .EQ. 1) THEN 
		PROP_RADIUS = radius
	END IF

!--- Cweight of blades, need to estimate Sigma and the hub Mass...
	PROP_BLADE_SIGMA = 2 ! kg/m²
	PROP_HUB_COEFF = 0.09 ! kg/m (assuming that the hub mass is proportional to the propeller radius)

	M_PROP = NR_BLADE * S * PROP_BLADE_SIGMA + radius * PROP_HUB_COEFF


	BLADE_ASPECT_RATIO = radius**2 / S

			CLOSE(20)

	END SUBROUTINE PROP_DATA_FINDER





	SUBROUTINE MOMENT_OF_INERTIA	
	USE MCOMMON
	IMPLICIT NONE

	REAL :: IFRAME, IMOTOR

	WRITE(*,*)'		Computing aircraft inertia' 

	IFRAME = 1/12 * M_FRAME * (2 * FRAME_SPAN)**2

	IMOTOR = (M_prop + M_MOTOR)* FRAME_SPAN**2

	I_YAW_TOTAL = (IFRAME + Nr_motor* IMOTOR)

!	WRITE(*,*)'I_YAW_TOTAL =',I_YAW_TOTAL,'kg.m^2'  !-debug


	END SUBROUTINE MOMENT_OF_INERTIA
