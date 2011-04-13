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
!--- First Check if the line starts with '!','#' if so skip this line ---!
	ELSE IF (LINE(1:1).EQ.'!') THEN
	GOTO 1000
	ELSE IF (LINE(1:1).EQ.'#') THEN
	GOTO 1000
	ELSE IF (LINE(2:2).EQ.' ') THEN
	GOTO 1000
	END IF

!	WRITE(*,*)' STATUS :',status
	CALL EXTRACT_LINE()
!	READ(60,*,iostat=status) !
1000	CONTINUE ! Just skipped the commented line !!!
	END DO
	CLOSE(60)

4000	FORMAT(A120)

5000	CONTINUE
	END SUBROUTINE 

	SUBROUTINE EXTRACT_LINE
	USE MCOMMON
	IMPLICIT NONE
!--- Find the first ":" char, assume that COMMAND ends there (to avoid issue with spaces or tab)---!
	KBLANK =INDEX(LINE,':')
	COMMAND = LINE(1:KBLANK-1)
	LEFT_ARGS = LINE(KBLANK+2:120)
	WRITE(*,*)'========================'        !---- Debug 
	WRITE(*,*)' COMMAND is : ',COMMAND          !---- Debug 
	WRITE(*,*)' LEFT_ARGS are : ',LEFT_ARGS     !---- Debug 
	WRITE(*,*)'========================'        !---- Debug 

!---CHARLES, we will only modify this part for all COMMANDS and their args---!
	SELECT CASE (COMMAND)	
	CASE ('MURAT')
		WRITE(*,*)' Reading 3 Real values !!! '     !---- Debug 
		READ(LEFT_ARGS,*, err = 6000)R1,R2,R3
		WRITE(*,*)' R1 is : ',R1      !---- Debug 
		WRITE(*,*)' R2 is : ',R2      !---- Debug 
		WRITE(*,*)' R3 is : ',R3      !---- Debug 
	CASE ('RHO')
		WRITE(*,*)' Reading 1 Real values !!! '  !---- Debug 
		READ(LEFT_ARGS,*, err = 6000)RHO
		WRITE(*,*)' RHO is : ',RHO    !---- Debug 
	CASE DEFAULT
		GOTO 7000	
	END SELECT



	
	GOTO 7000

!---If a COMMAND has been recognized but it's associated data could not be found---!
6000	WRITE(*,*)'!!!   ERROR   !!!'
	WRITE(*,*)' Could not get dat for ', COMMAND

7000	CONTINUE

	END SUBROUTINE



!	SUBROUTINE GETLINE(CMND,R,I,L,ERR)
!--- Reads the entire line, gets the command, 4 real, 4 integer, 4 logical values and error, we might not need it

!	END SUBROUTINE GETLINE

!	SUBROUTINE GOTOLINE(line_nr)
!	USE MCOMMON
!	IMPLICIT NONE

!	DO 1,line_nr
!	READ(60,*,iostat=status)
!	END DO

!	END SUBROUTINE GOTOLINE
