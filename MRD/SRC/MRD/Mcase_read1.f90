!-----------------------------------------------------------------------------|
!    Consists a part of MRD Program - Multi Rotor Vehicle Design, see MRD.f90 |
!    Copyright (C) 2011  Murat BRONZ & Charles PLACHOT                        |
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
!	WRITE(*,*)'========================'        !---- Debug 
!	WRITE(*,*)' COMMAND is : ',COMMAND          !---- Debug 
!	WRITE(*,*)' LEFT_ARGS are : ',LEFT_ARGS     !---- Debug 
!	WRITE(*,*)'========================'        !---- Debug 

!---CHARLES, we will only modify this part for all COMMANDS and their args---!
	SELECT CASE (COMMAND)	

	CASE ('RHO')
!		WRITE(*,*)' Reading 1 Real values !!! '  !---- Debug 
		READ(LEFT_ARGS,*, err = 6000)RHO
		WRITE(*,*)' RHO is : ',RHO    !---- Debug 
	CASE ('MU')
		READ(LEFT_ARGS,*, err = 6000)MU
		WRITE(*,*)' MU is : ',MU   !---- Debug
	CASE ('VSOUND')
		READ(LEFT_ARGS,*, err = 6000)VSOUND
		WRITE(*,*)' VSOUND is : ',VSOUND    !---- Debug
	CASE ('GRAVITATION')
		READ(LEFT_ARGS,*, err = 6000)GRAV_ACC
		WRITE(*,*)' GRAV_ACC is : ',GRAV_ACC    !---- Debug
	CASE ('MASS_PAYLOAD')
		READ(LEFT_ARGS,*, err = 6000)M_PAYLOAD
		WRITE(*,*)' M_PAYLOAD is : ',M_PAYLOAD    !---- Debug
	CASE ('MASS_AVIONICS')
		READ(LEFT_ARGS,*, err = 6000)M_AUTOP
		WRITE(*,*)' M_AUTOP is : ',M_AUTOP    !---- Debug
	CASE ('MASS_MISC')
		READ(LEFT_ARGS,*, err = 6000)M_MISC
		WRITE(*,*)' M_MISC is : ',M_MISC    !---- Debug
	CASE ('NR_MOTOR')
		READ(LEFT_ARGS,*, err = 6000)NR_MOTOR
		WRITE(*,*)' NR_MOTOR is : ',NR_MOTOR    !---- Debug
	CASE ('BATT_SPEC_ENERGY')
		READ(LEFT_ARGS,*, err = 6000)BATT_SPEC_NRG
		WRITE(*,*)' BATT_SPEC_NRG is : ',BATT_SPEC_NRG    !---- Debug
	CASE ('BATT_MASS_MULTIPLIER')
		READ(LEFT_ARGS,*, err = 6000)M_BATT_MIN, M_BATT_MAX, M_BATT_DELTA
		WRITE(*,*)' M_BATT is : ', M_BATT_MIN, M_BATT_MAX, M_BATT_DELTA  !---- Debug
	CASE ('BATT_MAX_VOLT')
		READ(LEFT_ARGS,*, err = 6000)BATT_MAX_VOLT
		WRITE(*,*)' BATT_MAX_VOLT is : ',BATT_MAX_VOLT    !---- Debug
	CASE ('FRAME_FIX_MASS')
		READ(LEFT_ARGS,*, err = 6000)M_FRAME_FIX
		WRITE(*,*)' M_FRAME_FIX is : ',M_FRAME_FIX    !---- Debug
	CASE ('FRAME_MAT')
		READ(LEFT_ARGS,*, err = 6000)FRAME_MAT
		WRITE(*,*)' FRAME_MAT is : ',FRAME_MAT    !---- Debug
	CASE ('FRAME_SHAPE')
		READ(LEFT_ARGS,*, err = 6000)FRAME_SHAPE
		WRITE(*,*)' FRAME_SHAPE is : ',FRAME_SHAPE    !---- Debug
	CASE ('FRAME_FIX_SIZE')
		READ(LEFT_ARGS,*, err = 6000)FRAME_FIX_SIZE
		WRITE(*,*)' FRAME_FIX_SIZE is : ',FRAME_FIX_SIZE    !---- Debug
	CASE ('MAX_FRAME_SIZE')
		READ(LEFT_ARGS,*, err = 6000)MAX_FRAME_SIZE
		WRITE(*,*)' MAX_FRAME_SIZE is : ',MAX_FRAME_SIZE    !---- Debug
	CASE ('TIP_CLRNC')
		READ(LEFT_ARGS,*, err = 6000)TIP_CLRNC
		WRITE(*,*)' TIP_CLRNC is : ',TIP_CLRNC    !---- Debug
	CASE ('AVIONICS_POWER')
		READ(LEFT_ARGS,*, err = 6000)AVIONICS_POWER
		WRITE(*,*)' AVIONICS_POWER is : ',AVIONICS_POWER    !---- Debug
	CASE ('PAYLOAD_POWER')
		READ(LEFT_ARGS,*, err = 6000)PAYLOAD_POWER
		WRITE(*,*)' PAYLOAD_POWER is : ',PAYLOAD_POWER    !---- Debug
	CASE ('MIN_TW_RATIO')
		READ(LEFT_ARGS,*, err = 6000)MIN_TW_RATIO
		WRITE(*,*)'MAX_STEADY_CURRENT ratio is : ',MAX_STEADY_CURRENT   !---- Debug
	CASE ('MAX_STEADY_CURRENT')
		READ(LEFT_ARGS,*, err = 6000)MAX_STEADY_CURRENT
		WRITE(*,*)'MAX_STEADY_CURRENT is : ',MAX_STEADY_CURRENT    !---- Debug
	CASE ('MAX_BURST_CURRENT')
		READ(LEFT_ARGS,*, err = 6000)MAX_BURST_CURRENT
		WRITE(*,*)'MAX_BURST_CURRENT is : ',MAX_BURST_CURRENT    !---- Debug
	CASE ('ENDURANCE_COEFF')
		READ(LEFT_ARGS,*, err = 6000)FTIME_COEFF
		WRITE(*,*)' ENDURANCE_COEFF is : ',FTIME_COEFF    !---- Debug
	CASE ('SIZE_COEFF')
		READ(LEFT_ARGS,*, err = 6000)SIZE_COEFF
		WRITE(*,*)' SIZE_COEFF is : ',SIZE_COEFF    !---- Debug
	CASE ('MANEUVRABILITY_COEFF')
		READ(LEFT_ARGS,*, err = 6000)TW_COEFF
		WRITE(*,*)' MANEUVRABILITY_COEFF is : ',TW_COEFF   !---- Debug
	CASE ('RANGE_COEFF')
		READ(LEFT_ARGS,*, err = 6000)RANGE_COEFF
		WRITE(*,*)' RANGE_COEFF is : ',RANGE_COEFF  !---- Debug
	CASE ('TRANSLATION_SPEED')
		READ(LEFT_ARGS,*, err = 6000)TRANSLATION_SPEED_MIN, TRANSLATION_SPEED_MAX, TRANSLATION_SPEED_DELTA
		WRITE(*,*)' TRANSLATION_SPEED is : ',TRANSLATION_SPEED_MIN, TRANSLATION_SPEED_MAX, TRANSLATION_SPEED_DELTA   !---- Debug
	CASE ('NR_BLADE')
		READ(LEFT_ARGS,*, err = 6000)NR_BLADE_MIN, NR_BLADE_MAX, DELTA_NR_BLADE
		WRITE(*,*)' NR_BLADE is : ', NR_BLADE_MIN, NR_BLADE_MAX, DELTA_NR_BLADE   !---- Debug
	CASE ('PROP_RADIUS')
		READ(LEFT_ARGS,*, err = 6000)PROP_RADIUS_MIN, PROP_RADIUS_MAX, PROP_RADIUS_DELTA
		WRITE(*,*)' PROP_RADIUS is : ', PROP_RADIUS_MIN, PROP_RADIUS_MAX, PROP_RADIUS_DELTA   !---- Debug
	CASE ('RPM')
		READ(LEFT_ARGS,*, err = 6000)RPM_MIN, RPM_MAX, RPM_DELTA
		WRITE(*,*)' RPM is : ',RPM_MIN, RPM_MAX, RPM_DELTA  !---- Debug
	CASE ('Thrust')
		READ(LEFT_ARGS,*, err = 6000)Thrust_MIN, Thrust_MAX, Thrust_DELTA
		WRITE(*,*)' Thrust is : ',Thrust_MIN, Thrust_MAX, Thrust_DELTA  !---- Debug
	CASE ('RUN_MODE')
		READ(LEFT_ARGS,*, err = 6000)RUN_MODE
		WRITE(*,*)' RUN_MODE is : ',RUN_MODE  !---- Debug
	CASE ('CONTROLLER_EFF')
		READ(LEFT_ARGS,*, err = 6000)CNTRLR_MIN_EFF,CNTRLR_MAX_EFF
		WRITE(*,*)' CNTRLR_EFF MIN MAX is : ',CNTRLR_MIN_EFF,CNTRLR_MAX_EFF  !---- Debug
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
