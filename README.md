#-------------------------------------------------------#
#            Double pendulum simulation                 #
#       Tomás Buratovich - Física computacional 2022    #
#-------------------------------------------------------#

This code compues the motion of the double pendulum implementing RungeKutta 4th order algorithm in Fortran 90 
and then performes a simulation of the motion using Pyhton libraries,

The code works as follows: 

the solver.f90 module contains the Runge Kutta 4th order subroutine (named rk4) that is used later by the main 
Fortan code. The main code i.e. main_double_pendulum.f90 computes de cinematic solution for the pendulum and stores the results inside 
3 files:

# DP_polar_coordinates.dat   ------> stores both R and theta coordinates for both pendulums
# DP_xy_coordinates.dat      ------> stores both x and y coordinates for both pendulums
# DP_xy_coordinates_util.dat ------> stores the same results as "DP_xy_coordinates.dat" but keeps and inferior number of data 
                                     to make simulation eassier later on
                                     
Finally "animation.py" takes the results from "DP_xy_coordinates_util.dat" and performs the simulation of the motion of the double pendulum.

# Executing instructions

To run the full code it only takes one step and it is runing in the console the bash code "run.sh" where every step of 
the calculation and simulation is concatenated
