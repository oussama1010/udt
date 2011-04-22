Multi Rotor Design Program Version 0.1
======================================
Murat Bronz @ 2011



Compilation of MRD
==================
Go to directory /BIN 
Edit the <Makefile> in order to change the fortran compiler if necessary. 
Type <make> to compile Qprop,Qmil and MRD all together in once.



USAGE of Program
================
Different CASEs should be created with a folder hierarchy like shown below. 

RUNS-->CASE1-->DATA-->MOTOR
   |     |       |--->PROPPELLER 
   |     |       |--->AIRFOIL
   |     |
   |     |->RESULTS-->CASE1a
   |              |-->CASE1b
   |              |-->CASE1c
   |
   |-->CASE2-->DATA-->MOTOR
         |       |--->PROPPELLER 
         |       |--->AIRFOIL
         |
         |->RESULTS-->CASE2a
                  |-->CASE2b
                  |-->CASE2c



  
In order to keep track of every execution, the results are automatically 
saved in a directory with the same CASE.run filename (CASE1a,CASE1b...).

EXECUTION
==========
The command to execute :
In /RUNS/CASE/ folder;
execute <../../BIN/mrd CASE.run> command.
This will read the CASE.run file as input, analyse and then save the results into
/RUNS/CASE/RESULTS/CASE/....


What is recorded in RESULTS
===========================










QUESTIONS/ANSWERS
=================
I will try to answer & document your questions here, so feel free to ask Qs :

Q1: When will the program be finish ?
A1: Soon :)

Q2: 


