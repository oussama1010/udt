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
	SUBROUTINE M_PROP_ANALYSE
	USE MCOMMON
	IMPLICIT NONE

	WRITE(*,*)' Existing Motor + Prop Analyse Subroutine '
!--- Loop for all motor and props... ---!

	indx_motor=indx_motor_ary(1)
	do while (indx_motor .le. indx_motor_ary(2))
	indx_prop=indx_prop_ary(1)
	do while (indx_prop .le.indx_prop_ary(2))
	M_BATT = M_BATT_MIN
	do while (M_BATT .le. M_BATT_MAX)
	

!According to the index number, get the motor name for Qprop calculations

	CALL Get_motor_name (indx_motor,motor_name)
	CALL Get_prop_name (indx_prop,prop_name)
	WRITE(*,*)'*****************	',trim(prop_name) ,'	', trim(motor_name), '		**************************************'
!	write(*,500) trim(qprop_infile), trim(motor_name), Speed(wcn), Thrust(wcn), trim(qprop_outfile)


!--- Just after having all the coeffs, Simulation needs to be called to calculte the mission...
	CALL M_SIMUL


	M_BATT = M_BATT + M_BATT_DELTA
	end do ! M_BATT loop
	indx_prop=indx_prop+1
	end do ! indx_prop loop
	indx_motor=indx_motor+1
	end do ! indx_motor loop



	END SUBROUTINE M_PROP_ANALYSE









