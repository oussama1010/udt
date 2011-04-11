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
	SUBROUTINE READ_CASE
	USE MCOMMON
	IMPLICIT NONE
	integer :: status
!Open the CASE file
!Read the command
!Decide which arguments to read
!Goto the line
!Read the arguments
!Goto next line and read the command
!Loop till the CASE file ends...


	OPEN(60,file='CASE.run', status='old',iostat=status)
!--- If the file doesnt exists tell to copy it from src...
		IF (status .ne. 0) then 
		WRITE(*,*)' CASE.run Variable initialization file doesnt exists...' 
		WRITE(*,*)' Copy it from /src directory...'
		CLOSE(60)
		GOTO 5000
		END IF
!--- File exists so read it...
	READ(60,*,iostat=status) ! This file contains the initial values of all the variables 
	READ(60,*,iostat=status) ! and coefficients that is going to be used in the program...
	READ(60,*,iostat=status) !

	CLOSE(60)
5000	CONTINUE

	END SUBROUTINE 



	SUBROUTINE GOTOLINE(line_nr)
	USE MCOMMON
	IMPLICIT NONE

	DO 1,line_nr
	READ(60,*,iostat=status)
	END DO

	END SUBROUTINE GOTOLINE
