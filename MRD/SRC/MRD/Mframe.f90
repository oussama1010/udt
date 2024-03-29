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
	SUBROUTINE MFRAME
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' 	Frame Building Subroutine '
!--- Fixed Frame Weight for Type 10---

	IF (M_frame_fix .Eq. 0) THEN 
		CALL ESTIMATE_MASS()
	ELSE 
		CALL ESTIMATE_MASS()
		M_frame = M_frame_fix
	END IF


	END SUBROUTINE MFRAME

	SUBROUTINE ESTIMATE_MASS
	USE MCOMMON
	IMPLICIT NONE

	Real ::  volume

	IF (FRAME_FIX_SIZE .eq. 0) THEN
		FRAME_SPAN = SQRT((2*PROP_RADIUS+TIP_CLRNC)**2/2)
			WRITE(*,*)'TIP_CLRNC',TIP_CLRNC
		IF (MAX_FRAME_SIZE .ne. 0 .and. 2*(FRAME_SPAN+PROP_RADIUS) .gt. MAX_FRAME_SIZE) THEN
			CANCEL_SIMUL = 1
		ELSE
			CANCEL_SIMUL = 0
		ENDIF
	ELSE 
		FRAME_SPAN = FRAME_FIX_SIZE/2 - PROP_RADIUS 
			WRITE(*,*)'FRAME_SPAN',FRAME_SPAN

		IF (2 * FRAME_SPAN**2 .le. (2*PROP_RADIUS+TIP_CLRNC)**2) THEN
			CANCEL_SIMUL = 1
		ELSE
			CANCEL_SIMUL = 0
		ENDIF

	END IF

	SELECT CASE (FRAME_SHAPE)
	CASE(0)
		SELECT CASE (FRAME_MAT)
			CASE(0)	
				volume = (0.1 *FRAME_SPAN)*(0.0533 *FRAME_SPAN)*FRAME_SPAN*4
				M_frame = volume *611.1+ 0.03
			CASE(1)	
				volume = (0.088 *FRAME_SPAN)*(0.035 *FRAME_SPAN)*FRAME_SPAN*4
				M_frame = volume *1.4/1000+ 0.03			! FRAME_SPAN = 24cm -> 110g
			CASE(2)
				volume = (0.2 *FRAME_SPAN)*(0.3 *FRAME_SPAN)*FRAME_SPAN*4
				M_frame = volume *0.333/1000+ 0.03
		END SELECT	
	CASE(1)
	END SELECT

!	WRITE(*,*)'M_FRAME', M_FRAME, FRAME_SPAN		!- debug
	END SUBROUTINE ESTIMATE_MASS
