import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
# import imageio

# Leer el archivo .dat con coordenadas cartesianas
data = np.loadtxt('DP_xy_coordinates_util.dat')

# Asignar las columnas correspondientes a las coordenadas x e y de cada péndulo
x1 = data[:, 1]
print(x1)
y1 = data[:, 2]
print(y1)
x2 = data[:, 3]
print(x2)
y2 = data[:, 4]
print(y2)

# Inicializar la figura y los ejes para la animación
fig, ax = plt.subplots()
ax.set_xlim(-15, 15)
ax.set_ylim(-1, 1)
line1, = ax.plot([], [], 'o-', lw=2)
line2, = ax.plot([], [], 'o-', lw=2)

# Definir la función update para actualizar la posición de los péndulos en cada frame
def update(num, x1, y1, x2, y2, line1, line2):
    line1.set_data([0, x1[num]], [0, y1[num]])
    line2.set_data([x1[num], x2[num]], [y1[num], y2[num]])
    return line1, line2

# Crear la animación
ani = animation.FuncAnimation(fig, update, frames=range(len(x1)), fargs=(x1, y1, x2, y2,line1, line2), blit=True, interval = 5)

print("using imagemagic protocol...")

# ani.save('animation.gif', writer='pillow')
plt.show()

"""

# Crear una lista vacía para almacenar los frames
frames = []

# Agregar cada frame de la animación a la lista
for i in range(len(ani.frames)):
    frames.append(ani.frames[i].to_image())

# Guardar la animación como un archivo .gif
imageio.mimsave('animation.gif', frames, 'GIF', fps=30)

"""