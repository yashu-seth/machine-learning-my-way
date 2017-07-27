from __future__ import division

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

a0, b0 = -2, 3
n = 30

X = np.random.rand(n)
Y = a0 * X + b0

lr = 0.01
epochs = 1000

#initial guess
a = -1 
b = 1
c = 4

Eh = []
ah, bh = [], []

def lin(X, a, b):
    return  a * X + b

f = lin

for i in range(epochs):

    E = (Y - f(X, a, b)) ** 2
    Eh.append(E.mean())

    dEdb =  2 * (f(X, a, b) - Y)
    dEda = dEdb * X

    a -= lr * dEda.mean()
    b -= lr * dEdb.mean()

    ah.append(a)
    bh.append(b)

red_patch = mpatches.Patch(color='red', label='a')
green_patch = mpatches.Patch(color='green', label='b')
blue_patch = mpatches.Patch(color='blue', label='Error')

plt.legend(handles=[blue_patch, red_patch, green_patch])

plt.plot(Eh, 'b')
plt.plot(ah, 'r')
plt.plot(bh, 'g')

plt.xlabel('epochs')

plt.show()
