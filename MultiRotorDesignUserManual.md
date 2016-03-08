# Multi Rotor Design - User Manual #

This page describes the use of MRD to design or simulate a multi-rotor UAV.

# Introduction #

MRD is an open source software developed in Enac's UAV department in order to design home made rotorcrafts, mainly quadrotors. MRD is based on the Qprop propeller simulator and on the Qmil design tool. It allows the user to simulate an existing vehicle, but also to run batch analysis and to plot the results in order to chose the optimum propeller, motor and battery for a given project.
For people capable of producing propellers, MRD is also capable of designing an optimized propeller to meet your mission constraints.

MRD is capable of determining the mass, size, flight time, thrust/weight ratio, power consumption, range of your airframe together with it's avionic and payload.


# Setup #

How to get MRD operational.

## What you need ##

MRD is developed on linux and thus is for now only available on linux based OS. However it should not be really difficult to port it to windows.

  1. That means that the first thing you need is a computer running with     Ubuntu or other Linux based OS.
  1. Install the gfortran-4.5 compiler with the command: **$ sudo apt-get install gfortran-4.5**
  1. Install svn (Subversion) in order to download keep the program up to date and submit your work if you want to contribute to this project. To do that use: **$ sudo apt-get install subversion**
  1. In order to have nice graphical output from the software you can install gnuplot using: **$ sudo apt-get install gnuplot**



## Setup step by step ##

You are now ready to download the source files and compile the software.

  1. First you need to create a folder wherever you want to have the program and move inside this folder. For example you can create a UDT folder using the command: **$ mkdir UDT**
  1. Move inside this folder using: **$ cd UDT**
  1. Then use the following command to register in google code to allow svn to download from the host : **$ svn checkout https://udt.googlecode.com/svn/trunk/ udt-read-only**
  1. To update the folder run the command : **$ svn up**

A udt folder will be created, it wil contain all you need to compile and run the software. Inside you will find:
  * **BIN** : the folder in which the binaries and the Makefile are
  * **SRC** : the folder containing the source files
  * **RUNS** : In which you store your propeller, motor, airfoil databases and your simulation configuration file. It will also contain the results once you have run some simulations.

To compile Go to directory /BIN
Edit the **Makefile** in order to change the Fortran compiler if necessary (default is the gfortran-4.5).
Type **$ make** to compile Qprop,Qmil and MRD all together.

MRD is now ready to run.

# Running the software #

## Modes available ##

MRD can be run in three different modes according to what you want to do and what kind of data you have. These three modes are:

> ### Qprop based simulation mode : ###
> This mode is meant to simulate multirotor vehicles using calls to Qprop in order to achieve higher precision and additional output. The software will combine propeller and motor models available. The difficult part here is to get an accurate propeller model which can be used by Qprop, i.e. span, cord law and airfoil characteristics. If your propeller is well known you should be able to find it's model using google otherwise you can find a modelisation procedure here http://web.mit.edu/drela/Public/web/qprop/prop_measure.pdf


> In addition the user can create engines model from his experiments or the manufacturer data. The model used is similar to the Qprop model but has two additional coefficients that are relevant for UAV design:

  * **The motor mass** in kg
  * **The maximum working power** in W

> ### Simplified simulation mode : ###
> In this mode the propeller is described by eight coefficients. This allow the user to create a model for an existing propeller just by doing some basic measurement on it. As it is not calling Qprop this mode is also much faster.
> The coefficients describing the aerodynamic efficiency of the propeller are:
    * **K0\_THRUST** in N (~0)
    * **K1\_THRUST** in N/RPM
    * **K2\_THRUST** in N/RPM^2
    * **K0\_TORQUE** in Nm (~0)
    * **K0\_TORQUE** in Nm/RPM
    * **K0\_TORQUE** in Nm/RPM^2

> These coeffiecients can be computed using excel for example and approximating the Thrust(RPM) and Torque(RPM) response with the following second order polynoms:

> Thrust(RPM) = K0\_THRUST + K1\_THRUST.RPM + K2\_THRUST.RPM^2

> Torque(RPM) = K0\_TORQUE + K1\_TORQUE.RPM + K2\_TORQUE.RPM^2

> The last coefficients are the **MASS** in kg and the **RADIUS** of the propeller in meter.


> ### Propeller design mode : ###
> This mode allow the user to generate propellers using Qmil according to some design constraints, simulate them and sort the ones meeting your mission constraints. The software will output a Qprop-type definition file which can allow you to design and produce an optimal propeller for your aircraft.

> The only inputs this mode need is the aerodynamic coefficients of the airfoils you want to use and the motors models.


## Running a Simulation ##

To run a simulation (or a design procedure) using MRD you need to create a Case folder containing the data and the incoming results. This folder needs to be located in the udt/MRD/RUNS folder. The easiest way to do it is to go inside the RUNS folder duplicate and rename the CASEexp folder. Call it for example SIMU1.

Then move inside this new folder. You will find a Case.run file, a DATA folder and a RESULTS folder. The Case.run file is the configuration file that will contain all the parameters of your simulation. Inside DATA/PROPELLER, DATA/MOTOR or DATA/SIMPLE\_PROPELLER just replace the existing models with the ones you want to use.

### Setting up a Simulation ###

> You can now get back to the Case.run file and open it. In this file you will find various parameters describing the mode you want to use, your aircraft shape, size, the way you want the results to be sorted...

> Here is an overview of all the parameters you need to tune for your simulation:

  * **RUN\_MODE** : define the mode in which the software will run. 0 will run the Simplified simulation mode using the SIMPLE\_PROPELLER models, 1 will run the Qprop based simulation and 2 will run the propeller design mode.


> System Weights

> This section define the mass of the different systems and payload onboard.

  * **MASS\_PAYLOAD** : define the mass in kg of the payload you intend to carry

  * **MASS\_AVIONICS** : define the mass in kg of your autopilot system (with sensors, modem, engine controllers...)

  * **MASS\_MISC** : various additional weight in kg


> Energy Source

> This section define the energy source of the aircraft.

  * **BATT\_SPEC\_ENERGY** : is the ratio of energy in Wh that contains your battery over it's mass. To approximate the energy contained in a battery just multiply it's capacity in Ah by the mean voltage during use (~11V fos a 3 cells Lipo).

  * **BATT\_MASS\_MULTIPLIER** : define the battery mass, you can enter here three values if you want the simulator to loop on the battery size. Enter the starting mass, the maximum mass, and delta. If you don't want to loop just use the same value for the minimum and maximum mass.

  * **BATT\_MAX\_VOLT** : the nominal voltage of your battery in V (12V for a 3 cells Lipo)

> Power Consumption

  * **AVIONICS\_POWER** : define the power consumption of the autopilot/stabilization system in W.

  * **PAYLOAD\_POWER** : power of payload in W

> Frame Specs

> This section define shape, size and mass of the aircraft naked frame.

  * **FRAME\_FIX\_MASS** : Is the mass in kg of your aircraft frame. If you let 0 the software will estimate it (for quad rotors only) according to the following parameters.

  * **NR\_MOTOR** : number of motors (and propellers) of your aircraft

  * **FRAME\_MAT** : Frame material. 0:carbon,1:aluminum,2:EPP

  * **FRAME\_SHAPE** : Define the shape of the frame. 0:cross, 1:round

  * **TIP\_CLRNCE** : Define the minimal clearance between the propellers tips

  * **FRAME\_FIX\_SIZE** : The span in m that will be used for your quad frame. The simulator will use this frame size and reject all the propellers to large to fit according to the tip clearance you set. Put 0 to let the simulator find the most compact frame for each propellers.

  * **MAX\_FRAME\_SIZE** : The maximal span of the quad frame. The simulator will reject all configurations exceeding this limit. Leave 0 to keep all configurations.

> Constraints & Checks

> This section define the criteria that have to be met for a configuration to be considered successful.

  * **MIN\_TW\_RATIO** : Define the minimum thrust/weight that a configuration have to reach. 2 should be a good compromise between maneuverability and endurance.

  * **MAX\_STEADY\_CURRENT** : Maximum current in A at which your engine controller is designed to operate continuously. All configuration exceeding this limit while hovering are rejected.

  * **MAX\_BURST\_CURRENT** : Maximum current in A that can be sustained by your controller for a short period of time. All configuration exceeding this limit while performing maximum thrust/weight ratio maneuver are rejected.

> Mission Scoring

> This section define the relative importance of different criteria in the output sorting.

  * **ENDURANCE\_COEFF** : Define the importance of endurance to sort the different configurations.

  * **SIZE\_COEFF** : Define the importance of the overall size to sort the different configurations, the smaller the better.

  * **MANEUVRABILITY\_COEFF** : Define the importance of maximum thrust/weight ratio to sort the different configurations.

  * **RANGE\_COEFF** : Define the importance of the range of a configuration to sort the different configurations. The range cannot be calculated in the Simplified simulation mode.

> Mission Settings

  * **TRANSLATION\_SPEED** : Define the translation speeds to simulate. Set minimum, maximum speed an delta in m/s. Keep min and max at the same value not to loop on the speed.

> Efficiencies

> Brushless engine controllers don't have a 100% efficiency, usually the higher efficiency is achieved at high RPM and the lowest at low RPM. This efficiency is difficult to modelise so for now we are just approximating it with a Linear function. You just need to give the program the minimum and maximum efficiency for low and high RPM respectively. If your are not sure just keep the values we set and that seems quite realistic for most controllers.

  * **CONTROLLER\_EFF** : Set the minimum and maximum controller efficiencies in %

### Launching the simulation & using the results ###

You have now edited the CASE.run file and compiled all the software executables. MRD is launched through a command line, the place from which you launch the command will impact the data it will use. Here is the recommended way of doing it:

  1. Move inside your experiment folder (the one containing your CASE.run file). This folder also contains your data folder, with your propellers/motor models.
  1. Launch the software using the CASE.run file using the command: **$ ../../BIN/mrd CASE.run**.
  1. You should see the software running for a while in the terminal and printing some debug. After this all your results will be created in the RESULTS folder. The main file is the CASE.txt which is a sorted list, according to your mission scoring coefficients, of all the suitable configurations.


## Running a Design Loop ##

To launch the software in design mode you will need to set a few parameters in addition to all those you set for the simulation mode. You will also need to feed the software with airfoil data files which are the files describing the airfoils that will be tested on your propellers. You can find example of these file in CASEexp/DATA/AIRFOIL and look at this link to create your own file : http://web.mit.edu/drela/Public/web/qprop/prop_measure.pdf