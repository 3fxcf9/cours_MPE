from math import cos, sin

import matplotlib.pyplot as plt
from matplotlib import animation, lines

v = 1.0
fd = 0.4
fs = 0.6
g = 9.81
k = 1
m = 1
xr = -1


def slip_pos(t):
    return (
        m * g / k * (fs - fd) * cos((k / m) * t)
        - m / k * (m * g * fd / k + xr) * sin((k / m) * t)
        + m * g * fd / k
        + xr
    )


def slip_velocitiy(t):
    return -g * (fs - fd) * sin((k / m) * t) - (m * g * fd / k + xr) * cos((k / m) * t)


xmax = fs * m * g / k + xr

step = 0.001
duration = 40

x = -3.0
X = [x]
V = [v]
T = [0.0]
stick = True
slip_at = 0

for i in range(1, int(duration / step)):
    if stick:
        x += v * step
        if x > xmax:
            stick = False
            slip_at = i
        V.append(v)
    else:
        t = (i - slip_at) * step
        x = slip_pos(t)
        vel = slip_velocitiy(t)
        V.append(vel)
        if vel > 0:
            stick = True

    X.append(x)
    T.append(i * step)


fig, (ax1, ax3) = plt.subplots(2, 1, sharex=False, figsize=(8, 10))
fig.suptitle("Stick-Slip", fontsize=16)

# Position
color = "tab:red"
ax1.set_title("Position et vitesse")
ax1.set_xlabel("temps (s)")
ax1.set_ylabel("position", color=color)
ax1.plot(T, X, color=color)
ax1.tick_params(axis="y", labelcolor=color)

# Vitesse
color = "tab:red"
ax2 = ax1.twinx()
color = "tab:green"
ax2.set_ylabel("vitesse", color=color)
ax2.plot(T, V, color=color)
ax2.tick_params(axis="y", labelcolor=color)

# Diagramme de phase

# Mettre les axes à (0,0)
ax3.spines["right"].set_position("zero")
ax3.spines["top"].set_position("zero")
ax3.spines["left"].set_color("none")
ax3.spines["bottom"].set_color("none")
ax3.yaxis.tick_right()
ax3.xaxis.tick_top()

ax3.set_title("Diagramme de phase")
ax3.set_xlabel("position")
ax3.set_ylabel("vitesse")
ax3.plot(X, V, color="tab:blue")

plt.tight_layout()
plt.show()


# ANIMATION
def draw_square(x):
    line1 = ax.plot([x, x + 0.1], [0, 0], color="black")[0]
    line2 = ax.plot([x, x + 0.1], [0.1, 0.1], color="black")[0]
    line3 = ax.plot([x, x], [0, 0.1], color="black")[0]
    line4 = ax.plot([x + 0.1, x + 0.1], [0, 0.1], color="black")[0]
    return line1, line2, line3, line4


def update_square(x, line1, line2, line3, line4):
    line1.set_data([x, x + 0.1], [0.001, 0.001])
    line2.set_data([x, x + 0.1], [0.1, 0.1])
    line3.set_data([x, x], [0.001, 0.1])
    line4.set_data([x + 0.1, x + 0.1], [0.001, 0.1])


fig, ax = plt.subplots()
fig.suptitle("Simulation", fontsize=16)
ax.spines["left"].set_color("none")
ax.spines["right"].set_color("none")
ax.spines["top"].set_color("none")
ax.spines["bottom"].set_color("none")
ax.spines["bottom"].set_position("zero")
ax.xaxis.tick_bottom()
ax.set_yticklabels([])
ax.set_yticks([])

ressort = ax.plot([], [])[0]
line1, line2, line3, line4 = draw_square(X[0])
support = ax.plot([min(X) - 1.5, max(X) + 1.5], [0, 0])[0]
plt.ylim(-0.3, 0.4)


def animate(i: int):
    x = X[20 * i]
    ressort.set_data([xr, x], [0.05, 0.05])
    update_square(x, line1, line2, line3, line4)
    return ressort, line1, line2, line3, line4


anim = animation.FuncAnimation(
    fig, animate, frames=int(len(T) / 20), interval=step, blit=True, repeat=False
)

plt.show()
