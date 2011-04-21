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
	SUBROUTINE MEXEC
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Execution Subroutine '

!--- Starting by generating the Frame
!	CALL MFRAME 
!--- MFRAME should have output the frame weight...
!--- Sum up all the components
!	CALL M_TOTAL_WEIGHT
!--- Assumed to have the final mass of the vehicle
!--- Calculate the required thrust for each motor...
!	THRUST(1) = TOTAL_WEIGHT/ NMOTOR  !convertion needed according to the units...

!--- Choose Analyse/Design mode and execute 
	IF(RUN_MODE .EQ. 2) THEN
	CALL M_PROP_ANALYSE
	ELSE IF (RUN_MODE .EQ. 1) THEN
	CALL M_PROP_DESIGN
	ELSE
	WRITE(*,*)' !!! RUN MODE is not defined !!!  Quitting...'
	STOP
	END IF

!--- Simulate the performance
!	CAll M_SIMUL

	END SUBROUTINE MEXEC
