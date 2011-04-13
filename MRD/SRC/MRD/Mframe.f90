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
	SUBROUTINE MFRAME
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Frame Building Subroutine '
!--- Fixed Frame Weight for Type 10---


	SELECT CASE(FRAME_TYPE)
	CASE (10)
		M_frame = M_frame_fix
	CASE (1) 
		CALL ESTIMATE_VOLUME()
	END SELECT


	END SUBROUTINE MFRAME

	SUBROUTINE ESTIMATE_VOLUME
	USE MCOMMON
	IMPLICIT NONE

	END SUBROUTINE ESTIMATE_VOLUME
