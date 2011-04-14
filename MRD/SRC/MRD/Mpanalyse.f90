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

!According to the index number, get the motor name for Qprop calculations

	CALL Get_motor_name (indx_motor,motor_name)
	CALL Get_prop_name (indx_prop,prop_name)
	WRITE(*,*)prop_name , motor_name
!	write(*,500) trim(qprop_infile), trim(motor_name), Speed(wcn), Thrust(wcn), trim(qprop_outfile)
!	write(*,*)qprop_in_command
	WRITE(qprop_in_command,500) trim(prop_name), trim(motor_name), Speed(wcn), Thrust(wcn), trim(qprop_outfile) 

	write(*,*) qprop_in_command
!500	Format ('../../BIN/qprop ',A,' ',A )

500	Format ('../../BIN/qprop',' ./DATA/PROPELLER/',A,' ./DATA/MOTOR/',A,' ',F5.2,' - - 0 ',F5.2,' > ',A )
	
	Call system (qprop_in_command)
! Read the results from Qprop out file
	Call qprop_read (Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, &
                                    Qprop_Volts, Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, & 
                                    Qprop_CP, Qprop_DV, Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, &
                                    Qprop_cd_avg, qprop_outfile, err_nr )
!--- Debug Print...
		write (*,*)
		write (*,*) 'Working Cond:  ', wcn
		write (*,*) 'Motor Name  :  ', motor_name
		write (*,*) 'Prop Name   :  ', prop_name 
		write (*,*) 'PROP Eff    :  ', Qprop_Eff_prop
		write (*,*) 'MOTOR Eff   :  ', Qprop_Eff_mot
		write (*,*) 'Total Eff   :  ', Qprop_Eff_total
		write (*,*) 'Torque      :  ', Qprop_Q
	 	write (*,*) 'Thrust      :  ', Qprop_T
	 	write (*,*)
!--- Just after having all the coeffs, Simulation needs to be called in Mexec.f90, to calculte the mission...

	indx_prop=indx_prop+1
	end do ! indx_prop loop
	indx_motor=indx_motor+1
	end do ! indx_motor loop


	END SUBROUTINE M_PROP_ANALYSE
