import math

import matplotlib.animation
import matplotlib.pyplot as plt
import numpy as np
from scipy.integrate import odeint

# Configuration
x0 = 1
v0 = [x0, 0, 0, 0]

# Simulation
earth_rotation_factor = 100
speed_multiplicator = 3
duration_seconds = 180
points_per_second = 10

# Constants
g = 9.8067
l = 67
lat = 49
day_duration_seconds = 86164
omega_0 = math.sqrt(g / l)
Omega_earth = earth_rotation_factor * 2 * math.pi / day_duration_seconds
Omega_corrected = Omega_earth * math.sin(math.radians(lat))

print("Afficher:\n(1) courbe statique\n(2) simulation\n(q) quitter")
while (choice := input("-> ")) not in ["1", "2", "q"]:
    print("Réponse invalide")

if choice == "q":
    exit()

# Run simulation

t = np.linspace(0, duration_seconds, duration_seconds * points_per_second)


def pendulum(v, t, Omega_corrected, omega_0):

    x, y, xd, yd = v

    xdd = 2 * Omega_corrected * yd - omega_0**2 * x
    ydd = -2 * Omega_corrected * xd - omega_0**2 * y
    dvdt = [xd, yd, xdd, ydd]

    return dvdt


sol = odeint(pendulum, v0, t, args=(Omega_corrected, omega_0))
xs, ys = sol[:, 0], sol[:, 1]


if choice == "1":
    plt.plot(sol[:, 0], sol[:, 1])
    plt.xlabel("x(m)")
    plt.ylabel("y(m)")
    plt.show()
elif choice == "2":
    fig, ax = plt.subplots()
    x, y = [], []
    g = ax.plot(x, y, c="#444455aa")[0]
    plt.xlabel("x(m)")
    plt.ylabel("y(m)")
    plt.xlim(-x0, x0)
    plt.ylim(-x0, x0)

    def color_list(i):
        gradient_points = math.floor((math.pi / omega_0) * points_per_second)
        if i < gradient_points:
            return [[j * (1 / gradient_points), 0, 0, 0.2] for j in range(1, i + 1)]
        return [[0, 0, 0, 0.2]] * (i - gradient_points) + [
            [j * (1 / gradient_points), 0, 0, 0.2]
            for j in range(1, gradient_points + 1)
        ]

    def animate(i):
        x.append(xs[i])
        y.append(ys[i])
        g.set_xdata(x)
        g.set_ydata(y)

    anim = matplotlib.animation.FuncAnimation(
        fig,
        animate,
        frames=duration_seconds * points_per_second,
        interval=1000 / (points_per_second * speed_multiplicator),
        repeat=False,
    )
    plt.show()
