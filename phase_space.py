

import numpy as np
import matplotlib.pyplot as plt

# Load the data from .dat file into a numpy array
data = np.loadtxt('DP_polar_coordinates.dat')

# Extract theta1 and dtheta1 columns
theta1 = data[:, 1]
dtheta1 = data[:, 2]

# Plot the phase space
plt.plot(theta1, dtheta1)

# Add labels and title to the plot
plt.xlabel('theta1 (rad)')
plt.ylabel('dtheta1/dt (rad/s)')
plt.title('Phase Space of Double Pendulum')

# Show the plot
plt.show()

#This code assumes that the .dat file has two columns, the first column being theta1 and the second column being dtheta1.




