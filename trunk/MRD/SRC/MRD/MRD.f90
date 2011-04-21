!-----------------------------------------------------------------------------|
!    MRD - Multi Rotor Vehicle Design, (by using Qprop & Qmil see /SRC/Qprop) |
!                                                                             |
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
	PROGRAM MRD
	USE MCOMMON
	IMPLICIT NONE

	VERSION = 0.1

	WRITE(*,*)' Welcome to MRD'
	WRITE(*,'(A,F3.1)')'  Version : ', VERSION
!--- Get command line arg for the CASE Filename ---
	CALL GETARG (1,COMMAND_LINE_ARG)
	CASE_FILE_NAME = TRIM(COMMAND_LINE_ARG)

!--- Initialization of CASE File ---
	CALL READ_CASE()
!=========================================
!--- Manual Initialization for now...
!--- Will be CALL READCASE() later, to read the CASE.txt input file...

	NR_MOTOR = 4   ! Number of motors
	NR_PROP = NR_MOTOR ! Number of Propellers
	NR_BLADE = 2 ! Blade number, will be array later
	RUN_MODE = 2  ! 1:design prop,2:use existing prop,3:...

	FRAME_TYPE = 1 ! Frame type 1,2,3,... will be defined 
	TIP_CLRNC = 0.01 ! Tip Clearance in meters

!	M_FRAME_FIX = 0.1 ! (kg) Fixed frame weight for FRAME_TYPE=10
!--- extra from MURAT ---!
	wcn = 1
	SPEED(1) = 0.06 !m/s
	THRUST(1) = 1.0 !N This is going to be changed after the total weight estimation!!!
	qprop_outfile = 'qprop_output.dat' ! this is the resultant filename coming out of Qprop...
!=========================================
!	WRITE(*,'(A,I2)')'  MOTOR NR : ', NR_MOTOR

!--- Motor,Airfoil and Propeller lists will be generated

	CALL SYSTEM('ls -B DATA/MOTOR > DATA/motor_name_list.txt')
	CALL SYSTEM('ls -B DATA/AIRFOIL > DATA/airfoil_name_list.txt')
	CALL SYSTEM('ls -B DATA/PROPELLER > DATA/propeller_name_list.txt')
	CALL SYSTEM("rm output.dat")
!######################################################

	indx_motor_ary(1)=1
	indx_prop_ary(1)=1
	call nmax_prop(n_prop)
	indx_prop_ary(2)=n_prop
	call nmax_motor(n_motor)
	indx_motor_ary(2)=n_motor
!	WRITE(*,*)' Number of motors in the list : ',n_motor
!	WRITE(*,*)' Number of props in the list : ',n_prop
!######################################################

!---Main Loops will be defined here and then EXECuted 1by1
	CALL MEXEC()

!	READ(*,5000)LINE
!	WRITE(*,*)LINE(1:1)
!	K = INDEX(LINE,'0')
!	KI = INDEX(LINE,'1')
!	KBLANK = INDEX(LINE,' ')
!	COMMAND = LINE(1:KBLANK)
!	LEFT_ARGS = LINE(KBLANK+1:120)
!	WRITE(*,*)' COMMAND is : ',COMMAND
!	WRITE(*,*)' LEFT_ARGS are : ',LEFT_ARGS
!	WRITE(*,*)' Index 0 of Line is : ',K
!	WRITE(*,*)' Index I of Line is : ',KI
!	WRITE(*,*)' Input Line is : ',LINE
!	WRITE(*,*)' Input Line 5th ch is : ',LINE(3:5)

	CALL SORT_OUTPUT()

5000	FORMAT(A120)

	END PROGRAM MRD 
