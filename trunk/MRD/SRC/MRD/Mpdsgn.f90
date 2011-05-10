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
	SUBROUTINE M_PROP_DESIGN
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Prop design Subroutine '

	indx_airfoil=indx_airfoil_ary(1)
	do while (indx_airfoil .le. indx_airfoil_ary(2))

	WRITE(*,*)'index, index max',indx_airfoil,indx_airfoil_ary(2)
	CALL Get_airfoil_spec(indx_airfoil)

	CALL Qmil_prop_write('prop test',2,30, CL0, CLA, CLmin, CLmax, CD0, CD2u, CD2l, CLCD0, REref, REexp, &
				0, 0.5, 1, 0.6, 0.45, 0.4, 0.015, 0.090, 0.1, 2000, 1.70, 0, 0, 0.2)
	

	indx_airfoil=indx_airfoil+1
	end do ! indx_prop loop



	END SUBROUTINE M_PROP_DESIGN
