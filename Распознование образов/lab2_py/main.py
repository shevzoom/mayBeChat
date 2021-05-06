from math import sqrt
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
from Color import COLORS
import statistics as s
from DataPoint import Point
from ReadFile import d

fig, ax = plt.subplots()
ax.axis([-10, 10, -10, 10])
ax.xaxis.set_major_locator(ticker.MultipleLocator(1))
ax.xaxis.set_minor_locator(ticker.MultipleLocator(1))
ax.yaxis.set_major_locator(ticker.MultipleLocator(1))
ax.yaxis.set_minor_locator(ticker.MultipleLocator(1))
ax.grid(which='major',color = 'k',linewidth = 0.5)
ax.grid(which='minor',
        color = 'gray',
        linestyle = ':')

fig.set_figwidth(7)
fig.set_figheight(7)

def D(point1, point2):
    return sqrt((point1.x - point2.x) ** 2 + (point1.y - point2.y) ** 2)

def dist_clusters(x_number, K, X, N):
    for i in range(x_number):
        min = D(N[0], X[i]) # расстояние между образами и центрами

        for j in range(K):
            temp = D(N[j], X[i])
            if temp < min:
                min = temp
                X[i].cluster_number = j

def df(x,y,x1,y1):
    return (x * x1 + y * y1) - 0.5 * (x1 ** 2 +y1 ** 2)

def drawPoint(x,y,K,N):
    min_df = df(x,y,N[0].x,N[0].y)

    for i in range(K):
        temp = df(x, y, N[i].x, N[i].y)
        if temp >= min_df:
            min_df = temp
        else:
            continue
        ax.scatter(x,y, color=COLORS[i], marker="^", s=200)

def iterations_ending(N, N_new):
    error = 0.1
    for i in range(N.__len__()):
        if (abs(N[i].x - N_new[i].x) > error) | (abs(N[i].y - N_new[i].y) > error):
            return False
    return True

def findNext(X, N):
    max_d = 0
    next_center = X[0]

    for i in range(N.__len__()):
        for j in range(X.__len__()):
            if X[j].cluster_number == i:
                temp = D(N[i], X[j])
                if max_d < temp:
                    max_d = temp
                    next_center = X[j]
    return next_center, max_d

def c(n, k):
    kk = 1
    nn = n
    for i in range(1, k):
        nn *= n - i
        kk *= i + 1
    return nn // kk

def getAverage_d(N):
    average_d = 0

    for i in range(N.__len__() - 1):
        average_d += D(N[i], N[i + 1])
    if N.__len__()<=2:
        return average_d / N.__len__()
    else:
        return average_d / c(N.__len__(), 2)

def draw(X, S, finished):
    temp_points_x = [];temp_points_y = [];temp_centers_x = [];temp_centers_y = []
    for i in range(S.__len__()):
        temp_points_x.clear(); temp_points_y.clear()
        temp_centers_x.append(S[i].x); temp_centers_y.append(S[i].y)
        for j in range(X.__len__()):
            if (X[j].cluster_number == i):
                temp_points_x.append(X[j].x)
                temp_points_y.append(X[j].y)
        plt.scatter(temp_points_x, temp_points_y, color=COLORS[i])
        ax.scatter(temp_centers_x[i],temp_centers_y[i],marker='*',c='black')
        plt.text(temp_centers_x[i],temp_centers_y[i], i+1, fontsize=16)

x_number = len(d['x'])
X = []; N = []
# print("Точек: ", x_number)

""" Шаг 1"""
for i in range(x_number):
    temp_point = Point(d['x'][i], d['y'][i])
    X.append(temp_point)

""" Произвольное назначение первого центра"""
N.append(X[5])

i = 0
while True:
    i += 1
    """ Шаг 2"""
    next_center, max_d = findNext(X, N)

    """ Шаг 3"""
    if max_d > getAverage_d(N) / 2:
        N.append(next_center)
        dist_clusters(x_number, N.__len__(), X, N)
        draw(X, N, False)
    else:
        N_new = N.copy()

        while True:
            dist_clusters(x_number, N.__len__(), X, N)
            draw(X, N, False)
            if iterations_ending(N, N_new):
                break
        break

print('i = ',i)

""" Точка """
p = Point(4, 5)
drawPoint(p.x, p.y, N.__len__(), N)

def line_intersection(line1, line2):
    xdiff = (line1[0][0] - line1[1][0], line2[0][0] - line2[1][0])
    ydiff = (line1[0][1] - line1[1][1], line2[0][1] - line2[1][1])

    def det(a, b):
        return a[0] * b[1] - a[1] * b[0]

    div = det(xdiff, ydiff)
    if div == 0:
       raise Exception('lines do not intersect')

    d = (det(*line1), det(*line2))
    x = det(d, xdiff) / div
    y = det(d, ydiff) / div
    return x, y

xx=[line_intersection((([((N[0].x+N[1].x)/2),((N[0].y+N[1].y)/2)] ),[0,((-1/((N[1].y-N[0].y)/(N[1].x-N[0].x))) * (0-((N[0].x+N[1].x)/2))+((N[0].y+N[1].y)/2))]),(([((N[0].x + N[2].x) / 2), ((N[0].y + N[2].y) / 2)]),[3, ((-1 / ((N[2].y - N[0].y) / (N[2].x - N[0].x))) * (3 - ((N[0].x + N[2].x) / 2)) + ((N[0].y + N[2].y ) / 2))]))[0],line_intersection((([((N[0].x+N[1].x)/2),((N[0].y+N[1].y)/2)] ),[0,((-1/((N[1].y-N[0].y)/(N[1].x-N[0].x))) * (0-((N[0].x+N[1].x)/2))+((N[0].y+N[1].y)/2))]),(([((N[0].x + N[2].x) / 2), ((N[0].y + N[2].y) / 2)]),[3, ((-1 / ((N[2].y - N[0].y) / (N[2].x - N[0].x))) * (3 - ((N[0].x + N[2].x) / 2)) + ((N[0].y + N[2].y ) / 2))]))[1]]
# print(xx)

def line(x3, zx1, zy1, zx2, zy2):

    if zx1 > 10 and zx2 < 10:
        xlist = np.linspace(-10, 10, 50)
        ylist = [((zx1 ** 2 + zy1 ** 2 - zx2 ** 2 - (zy2 ** 2)) / 2 - x3 * (zx1 - zx2)) / (zy1 - zy2) for x3 in xlist]
        plt.plot(xlist, ylist)
    else:
        x1list = np.linspace(-10, 10, 50)
        y1list = [((zx1 ** 2 + zy1 ** 2 - zx2 ** 2 - (zy2 ** 2)) / 2 - x3 * (zx1 - zx2)) / (zy1 - zy2) for x3 in x1list]
        plt.plot(x1list, y1list)

for i in range(len(N)):
    for j in range(len(N)):
        if i != j:
            line(p.x, N[i].x, N[i].y, N[j].x, N[j].y)
        else:
            break

draw(X, N, True)
plt.show()
print(d)