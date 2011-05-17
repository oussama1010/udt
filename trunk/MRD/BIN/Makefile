#----MAKEFILE FOR MRD TOOLCHAIN--------

QpSRC = ../SRC/Qprop
MSRC = ../SRC/MRD
MFLAGS = 
QFLAGS = -fdefault-double-8 -fdefault-real-8       
PROGS = mrd qprop qmil


MRDOBJS = Mcommon.o Mcase_read1.o Mfunctions.o Mframe.o Mpanalyse.o Mpdsgn.o \
Mweight.o Mexec.o MRD.o Moutput.o Msimul.o
#Mcase_read.o \

QPOBJS = qprop.o gvcalc.o cdfun.o tqcalc.o motor.o spline.o io.o qcget.o gaussn0.o
QMOBJS = qmil.o tpdes.o gvcalc.o cdfun.o tqcalc.o bnsolv.o spline.o io.o qcget.o

#------------------------------------
# Default Unix Fortran compiler
FC = gfortran-4.3
#FTNLIB = 

#------------------------------------
all:	$(PROGS)

mrd:	$(MRDOBJS)
	$(FC) -o mrd $(MRDOBJS) $(FTNLIB)

qprop:	$(QPOBJS)
	$(FC) -o qprop $(QPOBJS) $(FTNLIB)

qmil:	$(QMOBJS)
	$(FC) -o qmil $(QMOBJS) $(FTNLIB)

clean :
	-rm *.mod *~ *.*~
	-rm $(PROGS) $(QPOBJS) $(QMOBJS) $(MRDOBJS)


#-------MRD src files-----------------------
MRD.o :	$(MSRC)/MRD.f90
	$(FC) -c $(MFLAGS) $(MSRC)/MRD.f90
Mcommon.o : $(MSRC)/Mcommon.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Mcommon.f90
Mcase_read1.o : $(MSRC)/Mcase_read1.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Mcase_read1.f90
Mexec.o :	$(MSRC)/Mexec.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Mexec.f90
Mframe.o :	$(MSRC)/Mframe.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Mframe.f90
Mpdsgn.o :	$(MSRC)/Mpdsgn.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Mpdsgn.f90
Mpanalyse.o :	$(MSRC)/Mpanalyse.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Mpanalyse.f90
Mweight.o :	$(MSRC)/Mweight.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Mweight.f90
Mfunctions.o :	$(MSRC)/Mfunctions.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Mfunctions.f90
Moutput.o :	$(MSRC)/Moutput.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Moutput.f90
Msimul.o :	$(MSRC)/Msimul.f90
	$(FC) -c $(MFLAGS) $(MSRC)/Msimul.f90

#-------Qprop & Qmil src files--------------
qprop.o: $(QpSRC)/qprop.f $(QpSRC)/QDEF.INC
	$(FC) -c $(QFLAGS) $(QpSRC)/qprop.f

qmil.o: $(QpSRC)/qmil.f $(QpSRC)/QDEF.INC
	$(FC) -c $(QFLAGS) $(QpSRC)/qmil.f

tpdes.o: $(QpSRC)/tpdes.f
	$(FC) -c $(QFLAGS) $(QpSRC)/tpdes.f

gvcalc.o: $(QpSRC)/gvcalc.f
	$(FC) -c $(QFLAGS) $(QpSRC)/gvcalc.f

cdfun.o: $(QpSRC)/cdfun.f
	$(FC) -c $(QFLAGS) $(QpSRC)/cdfun.f

tqcalc.o: $(QpSRC)/tqcalc.f
	$(FC) -c $(QFLAGS) $(QpSRC)/tqcalc.f

motor.o: $(QpSRC)/motor.f
	$(FC) -c $(QFLAGS) $(QpSRC)/motor.f

bnsolv.o: $(QpSRC)/bnsolv.f
	$(FC) -c $(QFLAGS) $(QpSRC)/bnsolv.f

spline.o: $(QpSRC)/spline.f
	$(FC) -c $(QFLAGS) $(QpSRC)/spline.f

io.o: $(QpSRC)/io.f
	$(FC) -c $(QFLAGS) $(QpSRC)/io.f

qcget.o: $(QpSRC)/qcget.f
	$(FC) -c $(QFLAGS) $(QpSRC)/qcget.f

gaussn0.o: $(QpSRC)/gaussn0.f
	$(FC) -c $(QFLAGS) $(QpSRC)/gaussn0.f

