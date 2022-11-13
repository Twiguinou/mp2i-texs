import matplotlib.pyplot as plt

lbd = 2000000
lbd2 = lbd ** 2
mlbd_2 = -lbd * 2
w_02 = 81649 ** 2

def approx(t, xn, vn):
    return xn + t * vn

def approx_derivative(t, xn, vn):
    return vn + t * (-w_02 * xn + mlbd_2 * vn)

def calc_values(period, time, x0, v0):
    n = int(time / period)
    indexes = [0] * n
    values = [0] * n
    values[0] = x0
    prev_v = v0
    for i in range(1, n):
        values[i] = approx(period, values[i - 1], prev_v)
        prev_v = approx_derivative(period, values[i - 1], prev_v)
        indexes[i] = i * period
    return indexes, values

L = 0.001

from math import sqrt, exp
import numpy as np
def direct_method(t):
    vp = sqrt(lbd2 - w_02)
    return 1 / (2 * L * vp) * (exp((-lbd + vp) * t) - exp((-lbd - vp) * t))

T = 0.001
#idxs, vls = calc_values(T, 0.0028, 0, -1 / L)
#plt.plot(idxs, vls)

d = np.arange(0.0, 0.002, T)
dv = np.vectorize(direct_method)(d)
plt.plot(d, dv, 'b', d, dv, 'k')

plt.show()