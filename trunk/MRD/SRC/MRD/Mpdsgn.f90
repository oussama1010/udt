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
	SUBROUTINE M_PROP_DESIGN
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Prop design Subroutine '

	indx_airfoil=indx_airfoil_ary(1)
	do while (indx_airfoil .le. indx_airfoil_ary(2))
	NR_BLADE = NR_BLADE_MIN
	do while (NR_BLADE .le. NR_BLADE_MAX)


		CALL GENERATE_PROPELLER

		
	NR_BLADE = NR_BLADE + DELTA_NR_BLADE
	end do ! nr_blade loop
	indx_airfoil=indx_airfoil+1
	end do ! indx_airfoil loop



	END SUBROUTINE M_PROP_DESIGN

!################################################################################################################################
	SUBROUTINE GENERATE_PROPELLER
	USE MCOMMON
	IMPLICIT NONE

		CALL Get_airfoil_spec(indx_airfoil, Airfoil_name,CL0, CLA, CLmin, CLmax, CD0, CD2u, CD2l, CLCD0, REref, REexp)

!		propeller_candidate= 'temppropdata'
		WRITE(propeller_candidate,600)trim(Airfoil_name),NR_BLADE

600 	Format (A,'-B',I1)

		CALL Qmil_prop_write(propeller_candidate,NR_BLADE,30, CL0, CLA, CLmin, CLmax, CD0, CD2u, CD2l, CLCD0, &
				REref, REexp, 0, 0.5, 1.0, 0.6, 0.45, 0.4, 0.015, 0.090, 0.1, 3000.0, 1.70, 0, 0, 0.2)
	
!		WRITE(qmil_outfile,'(A,A,I3)')trim(Airfoil_name),'-B',NR_BLADE


		WRITE(qmil_in_command,500) trim(propeller_candidate),trim(propeller_candidate)

		write(*,*) qmil_in_command

500		Format ('../../BIN/qmil ',A,' ./RESULTS/Propeller/',A)


		Call system (qmil_in_command)
		Call system ('rm '//propeller_candidate)

	END SUBROUTINE GENERATE_PROPELLER
