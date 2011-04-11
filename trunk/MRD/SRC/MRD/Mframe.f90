	SUBROUTINE MFRAME
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Frame Building Subroutine '
!--- Fixed Frame Weight for Type 10---
	IF (FRAME_TYPE == 10) THEN
	M_frame = M_frame_fix

	END IF









	END SUBROUTINE MFRAME
