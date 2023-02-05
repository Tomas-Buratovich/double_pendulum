
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

def update(frame):
    line1.set_data([0,x1[frame]], [0,y1[frame]])
    line2.set_data([x1[frame], x2[frame]], [y1[frame], y2[frame]])
    time_text.set_text(f'time = {times[frame]:.2f}')
    return line1, line2, time_text

# Carga los datos desde el archivo .dat
data = np.loadtxt('DP_xy_coordinates_util.dat')
times = data[:, 0]
x1 = data[:, 1]
y1 = data[:, 2]
x2 = data[:, 3]
y2 = data[:, 4]

fig, ax = plt.subplots()
ax.set_xlim(-2, 2)
ax.set_ylim(-2, 2)
#line1, = ax.plot([], [], 'ro', markersize=10)
#line2, = ax.plot([], [], 'bo', markersize=10)
line1, = ax.plot([], [], 'o-', markersize=10, color='red')
line2, = ax.plot([], [], 'o-', markersize=10, color='blue')
time_text = ax.text(0.05, 0.9, '', transform=ax.transAxes)

ani = FuncAnimation(fig, update, frames=len(times), interval=3.5, blit=True)

plt.show()