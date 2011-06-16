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
	SUBROUTINE FILL_GNUPLOT_DATA
	USE MCOMMON
	IMPLICIT NONE

	open (60, file='gnuplot.dat', status='unknown',position="append")

	write (60,*)MAX_FLIGHT_TIME_FLOAT,M_BATT, TW_RATIO, MIN_TW_RATIO, AMPS, MAX_STEADY_CURRENT

	close (60)


	END SUBROUTINE FILL_GNUPLOT_DATA




	SUBROUTINE CREATE_GRAPH
	USE MCOMMON
	IMPLICIT NONE

	open (60, file='gnuplot.conf', status='new',position="append")

	write (60,*)'set term png '	
	write (60,*)'set xrange [', M_BATT_MIN,':', M_BATT_MAX,']'
	write (60,*)'set yrange [', 0,':', MAX_FLIGHT_TIME + 1,']'
	write (60,*)'set xlabel "Battery mass"'
	write (60,*)'set output "./RESULTS/GRAPHS/',prop_name(1:prop_cut-1),'-',trim(motor_name),'.png"'
	write (60,*)'plot "gnuplot.dat" using 2:1 title "Flight Time" w lines,', &
			' "gnuplot.dat" using 2:3 title "TW ratio" w lines, ', &
			' "gnuplot.dat" using 2:4 title "Min TW ratio" w lines, ', &
			' "gnuplot.dat" using 2:5 title "Motor input current to hover" w lines'

	close (60)

	CALL SYSTEM('/usr/bin/gnuplot gnuplot.conf')

	END SUBROUTINE CREATE_GRAPH
