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
	


SUBROUTINE M_TOTAL_WEIGHT
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Total Weight Subroutine ' 
	write (*,*)

!	CALL ENGINE_WEIGHT

	M_MOTOR = 0.02

!---- Summing all the mass...

	M_total = M_BATT + M_prop*Nr_prop + M_motor*Nr_motor + M_FRAME + M_PAYLOAD + M_AUTOP + M_MISC

!	WRITE(*,*)' M_BATT : ', M_BATT		!debug
!	WRITE(*,*)' M_prop: ', M_prop		!debug
!	WRITE(*,*)' M_motor: ', M_motor	!debug
!	WRITE(*,*)' M_FRAME: ', M_FRAME		!debug
!	WRITE(*,*)' M_PAYLOAD: ',M_PAYLOAD		!debug
!	WRITE(*,*)' M_AUTOP: ', M_AUTOP		!debug
!	WRITE(*,*)' Total quad Weight is: ', M_total		!debug


	END SUBROUTINE M_TOTAL_WEIGHT


!---Estimates the weight of a propeller from the data file...!
	SUBROUTINE PROP_DATA_FINDER	
	USE MCOMMON
	IMPLICIT NONE
	integer ::  status, i, j
	REAL :: radius, chord, radius1, chord1, S

	OPEN(20,file='DATA/PROPELLER/'//trim(prop_name), status='old', iostat=status)
!--- If the file doesnt exists tell to copy it from src...
	IF (status .ne. 0) then 
	WRITE(*,*)
	WRITE(*,*)' ******!!! WARNING !!!******' 
	WRITE(*,*)' PROPELLER DATA file doesnt exists...' 
	WRITE(*,*)' Check this file is inside the DATA/PROPELLER folder ;'
	WRITE(*,*)' ******!!! WARNING !!!******'
	WRITE(*,*)prop_name
	CLOSE(20)
	GOTO 5000
	END IF
!--- File exists so read it...
	DO i=1,50
		READ(20,4000,iostat=status)LINE
		IF(status .eq. -1) THEN
			CLOSE(20)
			exit
		END IF

!--- Find the number of blades...
		IF ( LINE( (INDEX(LINE,'!')+2) : (INDEX(LINE,'!')+9) ) .Eq. 'Nblades') THEN
			READ(LINE,*)NR_BLADE
!			WRITE(*,*)'The selected propeller has', NR_BLADE, ' blades'	!debug
		END IF

!--- Find the geometry of the blade and calculate its surface...
		IF ( LINE( 1 : 1) .Eq. '#') THEN
			DO j=1,15
				READ(20,*,iostat=status) radius1, chord1

				IF(status.eq.-1) THEN
					GOTO 5000
				END IF

				S= (radius1-radius)*(chord1+chord)*0.5+S
				radius = radius1
				chord = chord1
			END DO			
		END IF
	END DO

4000	FORMAT(A120)

5000	CONTINUE


	PROP_RADIUS = radius

!--- Cweight of blades, need to estimate Sigma and the hub Mass...
	PROP_BLADE_SIGMA = 0.0003 ! kg/cm²
	PROP_HUB_COEFF = 0.0001 ! kg/cm (assuming that the hub mass is proportional to the propeller radius)

	M_PROP = NR_BLADE * S * PROP_BLADE_SIGMA + radius * PROP_HUB_COEFF

!	WRITE(*,*)radius, chord
!	WRITE(*,*)'The propeller weights approximatively ', M_PROP, 'kg'	!debug

			CLOSE(20)

	END SUBROUTINE PROP_DATA_FINDER



