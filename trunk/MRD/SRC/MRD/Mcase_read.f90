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
	integer :: i, status
!Open the CASE file
!Read the command
!Decide which arguments to read
!Goto the line
!Read the arguments
!Goto next line and read the command
!Loop till the CASE file ends...


	OPEN(60,file=CASE_FILE_NAME, status='old',iostat=status)
!--- If the file doesnt exists tell to copy it from src...
		IF (status .ne. 0) then 
		WRITE(*,*)
		WRITE(*,*)' ******!!! WARNING !!!******' 
		WRITE(*,*)' CASE file doesnt exists...' 
		WRITE(*,*)' Run the program with a commandline argument as ;'
		WRITE(*,*)' <mrd CASE.run> and check the existence of CASE.run'
		WRITE(*,*)' ******!!! WARNING !!!******'
		WRITE(*,*)
		CLOSE(60)
		GOTO 5000
		END IF
!--- File exists so read it...
	DO i=1,1000
	READ(60,4000,iostat=status)LINE
	IF(status.eq.-1) THEN
	exit
	END IF
!--- First Check if the line starts with '!','C','#' if so skip this line ---!
!	WRITE(*,*)' STATUS :',status
	CALL EXTRACT_LINE()
!	READ(60,*,iostat=status) !

	END DO
	CLOSE(60)

4000	FORMAT(A120)

5000	CONTINUE
	END SUBROUTINE 

	SUBROUTINE EXTRACT_LINE
	USE MCOMMON
	IMPLICIT NONE
!--- Find the first blank char, assume that COMMAND ends there---!
	KBLANK = INDEX(LINE,' ')
	COMMAND = LINE(1:KBLANK)
	LEFT_ARGS = LINE(KBLANK+1:120)
	WRITE(*,*)'========================'        !---- Debug 
	WRITE(*,*)' COMMAND is : ',COMMAND          !---- Debug 
	WRITE(*,*)' LEFT_ARGS are : ',LEFT_ARGS     !---- Debug 
	WRITE(*,*)'========================'        !---- Debug 

!---CHARLES, we will only modify this part for all COMMANDS and their args---!
	IF(COMMAND .EQ. 'MURAT') THEN
	WRITE(*,*)' Reading 3 Real values !!! '     !---- Debug 
	READ(LEFT_ARGS,*)R1,R2,R3
	WRITE(*,*)' R1 is : ',R1      !---- Debug 
	WRITE(*,*)' R2 is : ',R2      !---- Debug 
	WRITE(*,*)' R3 is : ',R3      !---- Debug 
	ELSE IF(COMMAND .EQ. 'CHARLES') THEN
	WRITE(*,*)' Reading 3 Integer values !!! '  !---- Debug 
	READ(LEFT_ARGS,*)I1,I2,I3
	WRITE(*,*)' I1 is : ',I1      !---- Debug 
	WRITE(*,*)' I2 is : ',I2      !---- Debug 
	WRITE(*,*)' I3 is : ',I3      !---- Debug 
	END IF



	END SUBROUTINE

!	SUBROUTINE GETLINE(CMND,R,I,L,ERR)
!--- Reads the entire line, gets the command, 4 real, 4 integer, 4 logical values and error

!	END SUBROUTINE GETLINE

!	SUBROUTINE GOTOLINE(line_nr)
!	USE MCOMMON
!	IMPLICIT NONE

!	DO 1,line_nr
!	READ(60,*,iostat=status)
!	END DO

!	END SUBROUTINE GOTOLINE
