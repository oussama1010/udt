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

	Subroutine Qmil_prop_write (propeller_candidate, n_blade , Nout, CL0, CLA, CLmin, CLmax, &
                   CD0, CD2u, CD2l, CLCD0, REref, REexp, R1, R2, R3, RCL1, RCL2, RCL3, R_hub, R_tip, &
                   Speed, RPM, Thrust , Power , Ldes, FQdes) !, main )
	implicit none
	
	character(*), intent(in) :: propeller_candidate

	integer, intent(in) :: n_blade , Nout	!, main  ! Main Working Condition Number
	real, intent(in)  :: CL0, CLA, CLmin, CLmax, CD0, CD2u, CLCD0, CD2l, REref, REexp, R1, R2, &
			 R3, RCL1, RCL2, RCL3, R_hub, R_tip, Speed, RPM, Thrust , Power , Ldes, FQdes


!	main= ! IMPORTANT  , so that it takes the first wc as a main one...Thkn About It !!!

!	propeller_candidate = 'blade_deneme'
!	n_blade = 1
!	CL0 = -0.2700
!	CLA = 5.9000
!	CLmin = -1.2000
!	CLmax = 1.2000
!	CD0 = 0.02400
!	CD2u = 0.13000
!	CLCD0 = 0.0500
!	CD2l = 0.3
!	REref = 20000
!	REexp = -0.500
!	R1 = 0.0
!	R2 = 0.5
!	R3 = 1.0
!	RCL1 = 0.60 
!	RCL2 = 0.45
!	RCL3 = 0.40
!	R_hub = 0.015
!	R_tip = 0.090
!	Speed = 16
!	RPM = 10000.0
!	Thrust = 1.70 
!	Power = 0.0
!	Ldes = 0.0
!	FQdes = 0.2
!	Nout = 30

	write(*,*)'creating ',propeller_candidate
	write(*,*)
	open (60, file=propeller_candidate, status='unknown')
!			write (60,1000) propeller_candidate, n_blade, CL0, CLA, CLmin, CLmax, CD0, CD2u, CD2l,  &
!                                        CLCD0, REref, REexp, R1, R2, R3, RCL1, RCL2, RCL3, R_hub, R_tip, Speed(wcn), &
!                                        RPM, Thrust(wcn) , Power(wcn) , Ldes, FQdes, Nout
		write (60,1000) propeller_candidate, n_blade, CL0, CLA, CLmin, CLmax, CD0, CD2u, CD2l,  &
                                CLCD0, REref, REexp, R1, R2, R3, RCL1, RCL2, RCL3, R_hub, R_tip, Speed, &
                                RPM, Thrust , Power , Ldes, FQdes, Nout

1000	Format ( &
/A ,'	! Propeller Name' & 
//1X,I3,3X, '	! Number of Blades' &
//1X,2F8.4 , '	! CL0   CL_a' &
/1X,2F8.4 ,  '	! CLmin CLmax' &
//1X,3F9.5,F8.4, '	! CD0  CD2u  CD2l CLCD0' &
/1X,F10.1,F8.3,8X, '	! REref  REexp ' &
//1X,3F8.4, '   !  XIdes' &
/1X,3F8.4, '   !  CLdes' &
//1X,F8.3 , '	! Hub Radius (m)' &
/1X,F8.3 , '	! Tip Radius (m)' &
/1X,F8.1 , '	! Speed (m/s)'&
/1X,F12.1 , '	! RPM ' &
//1X,F8.2 , '	! Thrust (N) ( 0 if power  specified )' &
/1X,F8.2 , '	! Power (W) ( 0 if thrust specified )' &
//1X,F6.1 , F6.2 , '	 ! Ldes  FQdes ' & 
//1X,I3,8X , '	! Nout     number of output stations (optional)' )
	close (60)



	end subroutine
!################################################################################

	SUBROUTINE qprop_read (Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, &
                                    Qprop_Volts, Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, & 
                                    Qprop_CP, Qprop_DV, Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, &
                                    Qprop_cd_avg, qprop_outfile, err)
	implicit none

	integer :: i
	integer :: status
	
	integer,parameter :: n=40

	character(len=2) :: charc
	integer,intent(out) :: err
	character(len=25),intent(in) :: qprop_outfile
	real,intent(out) :: Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, &
                                    Qprop_Volts, Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, & 
                                    Qprop_CP, Qprop_DV, Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, &
                                    Qprop_cd_avg
	err = 1 !No errors...
		open(60,file=qprop_outfile,status='old',iostat=status)

				do i=1,19
				read(60,*,iostat=status) 
					if(status.eq.-1) then
					charc='er'
					exit
					end if
				enddo				
 			if (status.ne.-1) then
			read(60,*) charc 
			end if


		close(60)

	if (charc.ne.'#') goto 500

		open(60,file=qprop_outfile,status='old',iostat=status)

			read(60,*)
			read(60,*) 
			read(60,*)			
			read(60,*)
			read(60,*) 
			read(60,*)
			read(60,*)
			read(60,*) 
			read(60,*)
			read(60,*)
			read(60,*)			
			read(60,*)
			read(60,*) 
			read(60,*)
			read(60,*)
			read(60,*) 
			read(60,*)			
			read(60,*)
			read(60,*) 			
			read(60,*) charc , Qprop_V, Qprop_rpm, Qprop_Dbeta, Qprop_T, Qprop_Q, Qprop_Pshaft, &
                                    Qprop_Volts, Qprop_Amps, Qprop_Eff_mot, Qprop_Eff_prop, Qprop_Adv, Qprop_CT, & 
                                    Qprop_CP, Qprop_DV, Qprop_Eff_total, Qprop_P_elec, Qprop_P_prop, Qprop_cl_avg, &
                                    Qprop_cd_avg
			read(60,*)
				do i=1,n
				read(60,*,iostat=status) 
					if(status.eq.-1) exit
!				
				enddo				

			close(60)
500 Continue
	if(charc.ne.'#') err=2 ! Not converged...
	if(charc.ne.'#') write(*,*)'Error reading Qprop output, Qmil is not converged !!!'
	end subroutine qprop_read
!################################################################################
	Subroutine Get_airfoil_spec(indx, Airfoil_name,CL0, CLA, CLmin, CLmax, CD0, CD2u, CD2l, CLCD0, REref, REexp)
	implicit none
	character(len=25),intent(out) ::Airfoil_name
	real,intent(out) :: CL0, CLA, CLmin, CLmax, CD0, CD2u, CD2l, CLCD0, REref, REexp
	integer,intent(in) :: indx
	integer :: status, i
!	integer,parameter :: k=100
	character(len=25) :: Airfoil(100)


		open(70,file='./DATA/airfoil_name_list.txt',status='old',iostat=status)
	
				do i=1,100
				read(70,*,iostat=status) Airfoil(i)
					if(status.eq.-1) exit
				end do				

			close(70)
	
				WRITE(*,*)'Airfoil(indx)', Airfoil(indx)

		open(80,file='./DATA/AIRFOIL/'//Airfoil(indx),status='old',iostat=status)


				read(80,*)Airfoil_name
				read(80,*)CL0
				read(80,*)CLA
				read(80,*)CLmin
				read(80,*)CLmax
				read(80,*)CD0
				read(80,*)CD2u
				read(80,*)CD2l
				read(80,*)CLCD0
				read(80,*)REref
				read(80,*)REexp
				do i=1,100
				read(80,*,iostat=status)
					if(status.eq.-1) exit

				end do				

			close(80)
! To check if they are correctly read from the file.
	write(*,*)Airfoil(indx)
	write(*,*)CL0,CLA,CLmin,REexp


	end subroutine Get_airfoil_spec
!################################################################################
	Subroutine Get_motor_name (indx,Motor_name)
	implicit none
	integer,intent(in) :: indx
	integer :: status, i
	integer,parameter :: n=100
	character(len=25) :: Motor_name_array(100)
	character(len=25),intent(out) :: Motor_name


		open(80,file='./DATA/motor_name_list.txt',status='old',iostat=status)

				do i=1,n
				read(80,*,iostat=status) Motor_name_array(i)
					if(status.eq.-1) exit

				enddo				

			close(80)

	Motor_name=Motor_name_array(indx)
!	write(*,*)Motor_name
	end subroutine Get_motor_name
!################################################################################
	Subroutine Get_prop_name (indx,Prop_name)
	implicit none
	integer,intent(in) :: indx
	integer :: status, i
	integer,parameter :: n=100
	character(len=25) :: Prop_name_array(100)
	character(len=25),intent(out) :: Prop_name


		open(80,file='./DATA/propeller_name_list.txt',status='old',iostat=status)

				do i=1,n
				read(80,*,iostat=status) Prop_name_array(i)
					if(status.eq.-1) exit

				enddo				

			close(80)

	Prop_name=trim(Prop_name_array(indx))
!	write(*,*)' Propeller Name is : ',Prop_name
	end subroutine Get_prop_name
!################################################################################
	Subroutine Get_airfoil_name (indx,Airfoil_name)
	implicit none
	integer,intent(in) :: indx
	integer :: status, i
	integer,parameter :: n=100
	character(len=25) :: Airfoil_name_array(100)
	character(len=25),intent(out) :: Airfoil_name


		open(80,file='./DATA/airfoil_name_list.txt',status='old',iostat=status)

				do i=1,n
				read(80,*,iostat=status) Airfoil_name_array(i)
					if(status.eq.-1) exit

				enddo				

			close(80)

	Airfoil_name=Airfoil_name_array(indx)
!
	end subroutine Get_airfoil_name
!################################################################################
	Subroutine nmax_motor(n_motor)
	! Searches through the motor list and gives the total number of motors.
	implicit none
	integer,intent(out) :: n_motor
	integer :: status, i
	integer,parameter :: n=100

		open(81,file='./DATA/motor_name_list.txt',status='old',iostat=status)

				do i=1,n
				read(81,*,iostat=status) 
					if(status.eq.-1) exit
					n_motor=i
				enddo				

			close(81)
	end subroutine nmax_motor
!################################################################################
	Subroutine nmax_airfoil(n_airfoil)
	! Searches through the airfoil list and gives the total number of airfoils.
	implicit none
	integer,intent(out) :: n_airfoil
	integer :: status, i
	integer,parameter :: n=100

			open(81,file='./DATA/airfoil_name_list.txt',status='old',iostat=status)

				do i=1,n
				read(81,*,iostat=status) 
					if(status.eq.-1) exit
					n_airfoil=i
				enddo				

			close(81)
	end subroutine nmax_airfoil
!################################################################################
	Subroutine nmax_prop (n_prop)
	! Searches through the propeller list and gives the total number of propellers.
	implicit none
	integer,intent(out) :: n_prop
	integer :: status, i
	integer,parameter :: n=100

		open(81,file='./DATA/propeller_name_list.txt',status='old',iostat=status)

				do i=1,n
				read(81,*,iostat=status) 
					if(status.eq.-1) exit
					n_prop=i
				enddo				

			close(81)
	end Subroutine nmax_prop
!################################################################################
	Subroutine nmax_simple_prop (n_simple_prop)
	! Searches through the propeller list and gives the total number of propellers.
	implicit none
	integer,intent(out) :: n_simple_prop
	integer :: status, i
	integer,parameter :: n=100

		open(81,file='./DATA/simple_propeller_name_list.txt',status='old',iostat=status)

				do i=1,n
				read(81,*,iostat=status) 
					if(status.eq.-1) exit
					n_simple_prop=i
				enddo				

			close(81)
	end Subroutine nmax_simple_prop
!################################################################################

	Subroutine Get_prop_specs (indx,Prop_name,K0_THRUST,K1_THRUST,K2_THRUST, K0_TORQUE,K1_TORQUE, &
					K2_TORQUE, M_PROP, PROP_RADIUS)
	implicit none
	integer,intent(in) :: indx
	integer :: status, i
	integer,parameter :: n=100
	character(len=25) :: Prop_name_array(100)
	character(len=25),intent(out) :: Prop_name
	REAL, intent(out) ::  K0_THRUST,K1_THRUST,K2_THRUST, K0_TORQUE,K1_TORQUE,K2_TORQUE, M_PROP, PROP_RADIUS

		open(80,file='./DATA/simple_propeller_name_list.txt',status='old',iostat=status)

				do i=1,n
				read(80,*,iostat=status) Prop_name_array(i)
					if(status.eq.-1) exit

				enddo				

			close(80)

	Prop_name=Prop_name_array(indx)

	open(50,file='./DATA/SIMPLE_PROPELLER/'//Prop_name,status='old',iostat=status)

				read(50,*)
				read(50,*)K0_THRUST
				read(50,*)K1_THRUST
				read(50,*)K2_THRUST
				read(50,*)K0_TORQUE
				read(50,*)K1_TORQUE
				read(50,*)K2_TORQUE
				read(50,*)M_PROP
				read(50,*)PROP_RADIUS


	close(50)
! To check if they are correctly read from the file.
!	write(*,*)Prop_name					--debug
!	write(*,*)K_THRUST, K_TORQUE, M_PROP, PROP_RADIUS	--debug

	END SUBROUTINE Get_prop_specs

!################################################################################
	Subroutine Get_motor_specs (indx,Motor_name, R_MOTOR, I0_MOTOR, KV_MOTOR, M_MOTOR, MAX_POW_MOTOR)
	implicit none
	integer,intent(in) :: indx
	integer :: status, i
	integer,parameter :: n=100
	character(len=25) :: Motor_name_array(100)
	character(len=25),intent(out) :: Motor_name
	REAL, intent(out) ::  R_MOTOR, I0_MOTOR, KV_MOTOR, M_MOTOR, MAX_POW_MOTOR


		open(80,file='./DATA/motor_name_list.txt',status='old',iostat=status)

				do i=1,n
				read(80,*,iostat=status) Motor_name_array(i)
					if(status.eq.-1) exit

				enddo				

			close(80)

	Motor_name=Motor_name_array(indx)
!	write(*,*)Motor_name

	open(50,file='./DATA/MOTOR/'//Motor_name,status='old',iostat=status)

				read(50,*)
				read(50,*)
				read(50,*)
				read(50,*)
				read(50,*)
				read(50,*)R_MOTOR
				read(50,*)I0_MOTOR
				read(50,*)KV_MOTOR
				read(50,*)M_MOTOR
				read(50,*)MAX_POW_MOTOR

	close(50)

	end subroutine Get_motor_specs
