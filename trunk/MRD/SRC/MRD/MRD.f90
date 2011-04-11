	PROGRAM MRD
	USE MCOMMON
	IMPLICIT NONE

	VERSION = 0.1

	WRITE(*,*)' Welcome to MRD'
	WRITE(*,'(A,F3.1)')'  Version : ', VERSION
!=========================================
!--- Manual Initialization for now...
!--- Will be CALL READCASE() later, to read the CASE.txt input file...

	NR_MOTOR = 4   ! Number of motors
	NR_PROP = NR_MOTOR ! Number of Propellers
	NR_BLADE = 2 ! Blade number, will be array later
	RUN_MODE = 1  ! 1:design prop,2:use existing prop,3:...

	FRAME_TYPE = 1 ! Frame type 1,2,3,... will be defined 
	TIP_CLRNC = 0.01 ! Tip Clearance in meters

	M_FRAME_FIX = 0.1 ! (kg) Fixed frame weight for FRAME_TYPE=10

!=========================================
	WRITE(*,'(A,I2)')'  MOTOR NR : ', NR_MOTOR

!---Main Loops will be defined here and then EXECuted 1by1
	CALL MEXEC()

	END PROGRAM MRD 
