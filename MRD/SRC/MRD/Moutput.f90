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
	SUBROUTINE WRITE_OUTPUT(prop, motor, nrg, mass, hoverthrust, fpower, twratio, maxftime)
	USE MCOMMON
	IMPLICIT NONE

	character(len=25),intent(in) :: prop, motor

	Integer,intent(in) ::  maxftime

	real,intent(in) :: mass, nrg, hoverthrust, fpower, twratio
!Create an output file
!Write the simulation results inside one by one


	OPEN(10,file='output.dat', status='unknown',position="append")

	WRITE (10,*)'######################################################################'
	WRITE (10,*)'Motor:', trim(motor)
	WRITE (10,*)'Propeller:', trim(prop)
	WRITE (10,*)'Battery Energy:',  nrg, 'Wh'
	WRITE (10,*)'Flying weight:', mass, 'kg'
	WRITE (10,*)'Thrust to hover:', hoverthrust, 'N'
	WRITE (10,*)'Flight power:', fpower, 'W'
	WRITE (10,*)'Thrust/Weight ratio:', twratio
	WRITE (10,*)'Max flying time:', maxftime, 'min'
	WRITE (10,*)

	CLOSE (10)

	END SUBROUTINE WRITE_OUTPUT












	SUBROUTINE SORT_OUTPUT()
	USE MCOMMON
	IMPLICIT NONE
!generate a table from the raw output file
!Sort the results by endurance


	Integer :: maxtime, i, status

		WRITE(*,*)'Sorting output '    !---- Debug 

	OPEN(30,file='output.dat', status='old',iostat=status)
!--- If the file doesnt exists tell to copy it from src...
		IF (status .ne. 0) then 
		WRITE(*,*)
		WRITE(*,*)' ******!!! WARNING !!!******' 
		WRITE(*,*)'the outputfile cannot be found' 
		WRITE(*,*)' ******!!! WARNING !!!******'
		WRITE(*,*)
		CLOSE(30)
		GOTO 5000
		END IF
!--- File exists so read it...
	DO i=1,1000
	READ(30,4000,iostat=status)LINE
	IF(status.eq.-1) THEN
	exit
	ELSE IF (LINE(2:2).EQ.' ') THEN
	GOTO 1000
	END IF


	CALL EXTRACT_OUTPUT
	

1000	CONTINUE ! Just skipped the commented line !!!
	END DO

	CLOSE(30)

	CALL SYSTEM("rm output.dat")

	CALL SORT_TABLE

	DO i=1,n 
		CALL WRITE_OUTPUT(table1(2,i), table1(1,i), table2(1,i), table2(2,i), table2(3,i), table2(4,i), table2(5,i), &
		table3(1,i))
	END DO

4000	FORMAT(A120)

5000	CONTINUE


	END SUBROUTINE SORT_OUTPUT














	SUBROUTINE EXTRACT_OUTPUT()
	USE MCOMMON
	IMPLICIT NONE

!--- Find the first ":" char, assume that COMMAND ends there (to avoid issue with spaces or tab)---!
	KBLANK =INDEX(LINE,':')
	COMMAND = LINE(1:KBLANK-1)
	LEFT_ARGS = LINE(KBLANK+1:120)
!	WRITE(*,*)'========================'        !---- Debug 
!	WRITE(*,*)' COMMAND is : ',COMMAND          !---- Debug 
!	WRITE(*,*)' LEFT_ARGS are : ',LEFT_ARGS     !---- Debug 
!	WRITE(*,*)'========================'        !---- Debug 


	SELECT CASE (COMMAND)	

	CASE (' Motor')
		n = n + 1
		READ(LEFT_ARGS,*) table1(1,n)
!		WRITE (*,*) table1(1,n)
	CASE (' Propeller')
		READ(LEFT_ARGS,*) table1(2,n)
!		WRITE (*,*) table1(2,n)
	CASE (' Battery Energy')
		READ(LEFT_ARGS,*) table2(1,n)
	CASE (' Flying weight')
		READ(LEFT_ARGS,*) table2(2,n)
	CASE (' Thrust to hover')
		READ(LEFT_ARGS,*) table2(3,n)
	CASE (' Flight power')
		READ(LEFT_ARGS,*) table2(4,n)
	CASE (' Thrust/Weight ratio')
		READ(LEFT_ARGS,*) table2(5,n)
	CASE (' Max flying time')
		READ(LEFT_ARGS,*) table3(1,n)

	CASE DEFAULT
		GOTO 7000	
	END SELECT

	GOTO 7000

!---If a COMMAND has been recognized but it's associated data could not be found---!
6000	WRITE(*,*)'!!!   ERROR   !!!'
	WRITE(*,*)' Could not get dat for ', COMMAND

7000	CONTINUE

	END SUBROUTINE






	SUBROUTINE SORT_TABLE()
	USE MCOMMON
	IMPLICIT NONE

	Integer  :: indexmax, indexbegin, indexend, i 
	
	indexmax =0
	indexbegin = 1
	indexend = n

	DO While (indexbegin .Ne. n)
		indexmax = indexbegin
		DO i= indexbegin, n
			If ( table2(4,i) .Le. table2(4,indexmax)) Then
				indexmax = i
			END If
		END DO
		
		table1(1,n+1) = trim(table1(1,indexbegin))
		table1(2,n+1) = trim(table1(2,indexbegin))
		table2(1,n+1) = table2(1,indexbegin)
		table2(2,n+1) = table2(2,indexbegin)
		table2(3,n+1) = table2(3,indexbegin)
		table2(4,n+1) = table2(4,indexbegin)
		table2(5,n+1) = table2(5,indexbegin)
		table3(1,n+1) = table3(1,indexbegin)


		table1(1,indexbegin) = trim(table1(1,indexmax))
		table1(2,indexbegin) = trim(table1(2,indexmax))
		table2(1,indexbegin) = table2(1,indexmax)
		table2(2,indexbegin) = table2(2,indexmax)
		table2(3,indexbegin) = table2(3,indexmax)
		table2(4,indexbegin) = table2(4,indexmax)
		table2(5,indexbegin) = table2(5,indexmax)
		table3(1,indexbegin) = table3(1,indexmax)

		table1(1,indexmax) = trim(table1(1,n+1))
		table1(2,indexmax) = trim(table1(2,n+1))
		table2(1,indexmax) = table2(1,n+1)
		table2(2,indexmax) = table2(2,n+1)
		table2(3,indexmax) = table2(3,n+1)
		table2(4,indexmax) = table2(4,n+1)
		table2(5,indexmax) = table2(5,n+1)
		table3(1,indexmax) = table3(1,n+1)

		indexbegin = indexbegin + 1
		
	END DO



	END SUBROUTINE SORT_TABLE
