import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Circle

K = 1


def distance(C, x, y):
    xi, yi = C[0]
    return ((x - xi) ** 2 + (y - yi) ** 2) ** (1 / 2)


def potentiel(D, x, y):
    return sum([K * C[1] / distance(C, x, y) for C in D])


def champ(D, x, y):
    Ex, Ey, Er = 0, 0, 0
    for C in D:
        xi, yi = C[0]
        dx, dy = x - xi, y - yi
        r = distance(C, x, y)
        champ_radial = K * C[1] / (r**2)
        Ex += champ_radial * (dx / r)
        Ey += champ_radial * (dy / r)
        Er += champ_radial
    return Ex, Ey, Er


C1 = [[-1, 0], -1]
C2 = [[1, 0], 1]
D = [C1, C2]

X, Y = np.linspace(-3, 3, 100), np.linspace(-3, 3, 100)
XX, YY = np.meshgrid(X, Y)

# fig, (ax1, ax3) = plt.subplots(2, 1, sharex=False, figsize=(8, 10))
fig, ax = plt.subplots()
ax.set_aspect("equal")
ax.set_xlim(-3, 3)
ax.set_ylim(-3, 3)
for pos, q in D:
    ax.add_artist(Circle(pos, 0.05, color="red" if q > 0 else "blue"))

# Équipotentielles
Equipot = potentiel(D, XX, YY)
L = np.linspace(-5, 5, 25)
contour = ax.contour(
    X,
    Y,
    Equipot,
    levels=L,
    cmap="bwr",
)

# Lignes de champ
champX, champY, champR = champ(D, XX, YY)

stream = plt.streamplot(
    X,
    Y,
    champX,
    champY,
    color=np.log(np.fabs(champR) + 1e-9),
    cmap="plasma",
    # start_points=[C1[0], C2[0]],
)
stream.lines.set_alpha(0.4)


fig.suptitle("Équipotentielles et lignes de champ pour un doublet", fontsize=16)
plt.show()
