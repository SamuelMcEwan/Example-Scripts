import numpy as np, random, matplotlib.pyplot as plt, time
from scipy.spatial import distance

cities = 1000
df = np.array([[random.randrange(1000) for x in range(2)] for x in range(cities)]) 
order = random.sample(range(cities), cities)
plt.plot(df[order,0], df[order,1], '-o') 
plt.show()

dist_mtx = distance.cdist(df, df, 'euclidean')

def t_distance(order):
    total_distance = 0
    for x in range(len(order)-1): 
        total_distance = total_distance + dist_mtx[order[x], order[x+1]]
    return(total_distance)

def swap(order, dis):
    [pos1, pos2] = sorted(random.sample(range(1,cities),2))
    ord = order[:pos1] + order[pos1:pos2][::-1] + order[pos2:]
    newdis = dis - dist_mtx[order[pos1-1], order[pos1]] - dist_mtx[order[pos2-1], order[pos2]] + dist_mtx[order[pos1-1], order[pos2-1]] + dist_mtx[order[pos2], order[pos1]]
    res = dict(zip(["ord", "newdis"],[ord, newdis]))
    return(res)


fig = plt.gcf()
order = random.sample(range(cities), cities)
E = t_distance(order)
iter = 1
Error = []
startTime = time.perf_counter()
runTime = 30
plotTime = 0.5
plot_times = np.round(np.arange(0,runTime,plotTime), 2)
display_plot = np.array([True for x in range(int(runTime/plotTime))])
while (time.perf_counter() - startTime < runTime):
    opt2 = swap(order = order, dis = E)
    newOrder = opt2['ord']
    newE = opt2['newdis']
    if(newE < E):
        E = newE
        order = newOrder
        iter = iter + 1
        Error.append(E)
        time_since = time.perf_counter()-startTime
        if(sum(display_plot[plot_times == round(time_since,1)]) > 0):
            fig.clear()
            plt.plot(df[order,0], df[order,1], '-o') # - for line, and o for point. Simple!  o-g is the same as -og which is line, point, green 
            plt.title('TSP: Time = {}s, Distance = {}m'.format(round(time_since,1), round(E)) , fontsize=14, fontweight='bold')
            plt.savefig('Plot Subdirectory/Test{}.png'.format(1000*round(time_since,4)))
            display_plot[plot_times == round(time_since,1)] = False
    

plt.plot([x for x in range(1, iter)], Error)
plt.show()
