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


	SUBROUTINE CREATE_OUTPUT_FILE()
	USE MCOMMON
	IMPLICIT NONE


!Create an output file from the result table


!the rows have been sorted according to their respective mission scores
	DO k=1, n, 1

	OPEN(10,file='output.dat', status='unknown',position="append")
	WRITE (10,*)'Config', k, '		MISSION SCORE :', table3(2,k)
	WRITE (10,*)'##############################  ',table1(1,k),table1(2,k),'  #######################################'
	WRITE (10,'(A,F5.3,A)') ' Battery Mass test        : ',  table2(7,k), ' kg'
	WRITE (10,'(A,F5.2,A)') ' Battery Energy           : ',  table2(1,k), ' Wh'
	WRITE (10,'(A,F6.3,A)') ' Flying weight            : ',  table2(2,k), ' kg'
	WRITE (10,'(A,F4.1,A)') ' Flying Speed             : ',  table2(9,k), ' m/s'
	WRITE (10,'(A,F7.0,A)') ' Maximal Range            : ',  table2(10,k), ' m'
	WRITE (10,'(A,F4.1,A)') ' Frame size               : ', 2 * 100 * table2(6,k), ' cm'
	WRITE (10,'(A,F5.3,A)') ' Frame weight             : ',  table2(8,k), ' kg'
	WRITE (10,'(A,F5.2,A)') ' Thrust to hover          : ',  table2(3,k), ' N'
	WRITE (10,'(A,F6.2,A)') ' Flight power             : ',  table2(4,k), ' W'
	WRITE (10,'(A,F3.1,A)') ' Thrust/Weight ratio      : ',  table2(5,k)
	WRITE (10,'(A,F6.1,A)') ' Yaw angular acceleration : ',  table4(1,k) * 180/3.14,' deg/s²'
	WRITE (10,'(A,I3,A)')   'Max flying time           : ', table3(1,k), ' min'
	WRITE (10,*)
	WRITE (10,*)
	END DO
	CLOSE (10)

	DO k=1, n, 1
	OPEN(20,file='raw_output.dat', status='unknown',position="append")
!---Batt mass, FlightTime
	WRITE (20,*) table2(7,k),table3(1,k)
	CLOSE (20)
	END DO


	WRITE(*,*)'See output.dat file for results'

	END SUBROUTINE CREATE_OUTPUT_FILE












	SUBROUTINE CREATE_OUTPUT_TABLE
	USE MCOMMON
	IMPLICIT NONE

!add results of a simulation inside a sorted table
	Integer ::  i, j
	
	CALL CALCULATE_MISSION_SCORE

	WRITE(*,*)'adding results to ouptput table'

! the results are sorted by mission score
	DO i=1,n+1
		! all the lines showing a lower mission score are shifted one row down
		IF (table3(2,i) .Le. MISSION_SCORE) THEN
			DO j=n+1,i,-1
				table1(1,j+1) = trim(table1(1,j))
				table1(2,j+1) = trim(table1(2,j))
				table2(1,j+1) = table2(1,j)
				table2(2,j+1) = table2(2,j)
				table2(3,j+1) = table2(3,j)
				table2(4,j+1) = table2(4,j)
				table2(5,j+1) = table2(5,j)
				table2(6,j+1) = table2(6,j)
				table2(7,j+1) = table2(7,j)
				table2(8,j+1) = table2(8,j)
				table2(9,j+1) = table2(9,j)
				table2(10,j+1) = table2(10,j)
				table4(1,j+1) = table4(1,j)
				table3(1,j+1) = table3(1,j)
				table3(2,j+1) = table3(2,j)
			END DO
		! the new row is inserted in the free space created
			table1(1,i) = trim(prop_name)
			table1(2,i) = trim(motor_name)
			table2(1,i) = BATT_SPEC_NRG * M_BATT
			table2(2,i) =  M_TOTAL
			table2(3,i) = Thrust(wcn)
			table2(4,i) = TOTAL_FLYING_POWER
			table2(5,i) = TW_RATIO
			table2(6,i) = FRAME_SPAN
			table2(7,i) = M_BATT
			table2(8,i) = M_FRAME
			table2(9,i) = TRANSLATION_SPEED
			table2(10,i) = MAX_RANGE
			table4(1,i) = YAW_ANGULAR_ACCELERATION
			table3(1,i) = MAX_FLIGHT_TIME
			table3(2,i) = MISSION_SCORE
		! the number of rows is updated
			n = n + 1
			
			EXIT
		END IF
	END DO


	END SUBROUTINE CREATE_OUTPUT_TABLE






	SUBROUTINE CALCULATE_MISSION_SCORE
	USE MCOMMON
	IMPLICIT NONE


	MISSION_SCORE=(FTIME_COEFF*MAX_FLIGHT_TIME)/15+SIZE_COEFF*(10/FRAME_SPAN)+TW_COEFF*(TW_RATIO/3)+RANGE_COEFF*MAX_RANGE/5000

	END SUBROUTINE CALCULATE_MISSION_SCORE
