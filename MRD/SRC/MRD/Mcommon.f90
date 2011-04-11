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

	INTEGER :: i

	INTEGER :: 	NR_MOTOR,	&
			NR_PROP,	&
			NR_BLADE,	&
			RUN_MODE,	&
			FRAME_TYPE


	REAL :: TIP_CLRNC


	REAL :: M_FRAME,	&
		M_FRAME_FIX,	&
		M_PROP,		&
		M_BATT,		&
		M_MOTOR,	&
		M_TOTAL

	END MODULE MCOMMON
