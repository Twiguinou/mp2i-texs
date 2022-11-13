import matplotlib.pyplot as plt

lbd = 2000000
lbd2 = lbd ** 2
mlbd_2 = -lbd * 2
w_02 = 81649 ** 2

def calculate_next(t, in0, in1):
    return (2 - 2*lbd*t)*in1 - (1 - 2*lbd*t + t**2*w_02)*in0

def calc_values(period, time, i0, i1):
    n = int(time / period)
    indexes = [0] * n
    indexes[1] = period
    values = [0] * n
    values[0] = i0
    values[1] = i1
    for i in range(2, n):
        values[i] = calculate_next(period, values[i - 2], values[i - 1])
        indexes[i] = i * period
    return indexes, values

L = 0.001

from math import sqrt, exp
import numpy as np
def direct_method(t):
    vp = sqrt(lbd2 - w_02)
    return -1 / (2 * L * vp) * (exp((-lbd + vp) * t) - exp((-lbd - vp) * t))

T = 0.00000001
idxs, vls = calc_values(T, 0.0028, 0, -T / L)
plt.plot(idxs, vls)

d = np.arange(0.0, 0.002, T)
dv = np.vectorize(direct_method)(d)
plt.plot(d, dv, 'b', d, dv, 'k')

plt.show()