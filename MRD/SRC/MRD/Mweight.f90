	SUBROUTINE M_TOTAL_WEIGHT
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Total Weight Subroutine '
!---- Summing all the mass...

	M_total = M_batt + M_prop*Nr_prop + M_motor*Nr_motor


	END SUBROUTINE M_TOTAL_WEIGHT
