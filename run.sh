#!/bin/bash

echo "------------------------------------------------"
echo "| Starting compilation of Double Pendulum code |"
echo "------------------------------------------------"

gfortran -Wall -pedantic -c solver.f90
gfortran -Wall -pedantic -c main_double_pendulum.f90
gfortran -Wall -pedantic -o double_pendulum.x main_double_pendulum.o solver.o 

echo "-----------------------------------------------"
echo "|    Finished compiling Double Pendulum code  |"
echo "-----------------------------------------------"

echo "-----------------------------------------------"
echo "|          Deleting unnecessary files         |"
echo "-----------------------------------------------"

rm *.mod && rm *.o

echo "-----------------------------------------------"
echo "|   Executing the Double Pendulum simulation  |"
echo "-----------------------------------------------"

./double_pendulum.x

echo "-----------------------------------------------"
echo "|     Starting to run Pyhton simulation       |"
echo "-----------------------------------------------"

python3 animation_updated.py

rm *.dat






















