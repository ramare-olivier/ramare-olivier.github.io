############# Common handler #######################""

def fC(p):
    return([p[0]/(p[0]+2), (p[1]+1)/(p[0]+2)])

def gD(p):
    return([(p[1]+1)/(p[0]+2), p[0]/(p[0]+2)])

def putPoint(p, where, figsize = 4, modifier = "NE"):
    [u, v] = p
    aux = points(p, zorder=2)
    numu = numerator(u)
    denu = denominator(u)
    numv = numerator(v)
    denv = denominator(v)
    if modifier == "NE":
        signu = +1
        signv = +1
    elif modifier == "NW":
        signu = -1
        signv = +1
    elif modifier == "SW":
        signu = -1
        signv = -1
    else:
        signu = +1
        signv = -1
    tag = text((r'$(\frac{0}{1}, \frac{2}{3})$').format({numu}, {denu}, {numv}, {denv}),
               (u+signu*0.22/myfigsize, v+signv*0.16/myfigsize))
    return(aux + tag)

def reduce(asortedlist):
    maxk = len(asortedlist)-1
    for k in range(1, maxk+1):
        if asortedlist[maxk-k] == asortedlist[maxk-k+1]:
            asortedlist.pop(maxk-k+1)
    return(asortedlist)

############ Building of the C-curve from up #############################""

def transformSquare(InitDomain, nbsteps, myzorder, doplot = True, coloroffset = 0):
    # When doplot = False, be sure nbsteps >0
    acolor = Color(0/255, (70 + 40*(nbsteps + coloroffset))/255, 200/255)
    DomainfC = list(map(fC, InitDomain))
    DomaingD = list(map(gD, InitDomain))
    ToPlot = Graphics() # empty graphical object
    if doplot:
        ToPlot = polygon(InitDomain, color = acolor, zorder = myzorder)
        if nbsteps > 0:
            ToPlot += transformSquare(DomainfC, nbsteps-1, myzorder+1, True, coloroffset)
            ToPlot += transformSquare(DomaingD, nbsteps-1, myzorder+1, True, coloroffset)
    else:
        if nbsteps > 0:
            ToPlot = transformSquare(DomainfC, nbsteps-1, myzorder+1, True, coloroffset)
            ToPlot += transformSquare(DomaingD, nbsteps-1, myzorder+1, True, coloroffset)
    return(ToPlot)

# show(transformSquare([[0,0], [0,1], [1,1], [1,0]], 3, 2, False, 70), frame=True, figsize = 50)
# show(transformSquare([[0,0], [0,2], [2,2], [2,0]], 3, 2, False, 70), frame=True, figsize = 8)



############ Building of the C-curve from inside #############################""

from sage.combinat.cartesian_product import CartesianProduct_iters
from sage.geometry.polyhedron.plot import cyclic_sort_vertices_2d

def plotCS(S, nbsteps, myfigsize = 16, Init = [], WithPoints = False):
    # Init is a sequence 1, 0, 0, 1 ...
    # The 1 is for fC, the 0 for gD : we start from this point.
    S = list(S)
    #print(S)
    S.extend(list(map(lambda x: (x[1], x[0]), S)))
    S = sorted(S, key = lambda x: x[0])
    # print(S)
    ListPoints = reduce(S)
    for k in range(nbsteps):
        ListPoints += list(map(fC, ListPoints)) + list(map(gD, ListPoints))
        ListPoints.sort(key = lambda x: x[0])
        ListPoints = reduce(ListPoints)
        #print(ListPoints)

    for k in range(len(Init)):
        if Init[len(Init)-k-1] == 1:
            ListPoints =  list(map(fC, ListPoints))
        else:
            ListPoints =  list(map(gD, ListPoints))
            
    Tag = (r'The curve $\mathcal{}$ from words of length {}').format({'C'}, nbsteps)
    ToPlot = list_plot(ListPoints, frame = True, color = 'green',
                       plotjoined = True, figsize = myfigsize,
                       axes_labels = ['$u$ axis','$v$ axis'],
                       legend_label = Tag)
    if WithPoints:
        ToPlot += list_plot(ListPoints, color = 'royalblue', #size=4,
                            plotjoined = False, figsize = myfigsize)
    return([ListPoints, ToPlot])

def plotC(nbsteps, myfigsize = 16, Init = [], WithPoints = False):
    # Init is a sequence 1, 0, 0, 1 ...
    # The 1 is for fC, the 0 for gD : we start from this point.
    return(plotCS([[0,1],[1,0]], nbsteps, myfigsize, Init, WithPoints))

def plotCW(nbsteps, myfigsize = 16, Init = [], WithPoints = False):
    # Init is a sequence 1, 0, 0, 1 ...
    # The 1 is for fC, the 0 for gD : we start from this point.
    return(plotCS([[0,1],[1,0],[89/260, 89/260]], nbsteps, myfigsize, Init, WithPoints))

############ Building of the C-curve from inside, II #############################""

def getPerronFrobeniusEigenvector(MM):
    myE = MM.eigenvectors_right()
    choice = myE[0]
    for vec in myE:
        if choice[0] < vec[0]:
            choice = vec
    return(choice[1][0])

def plotCWithPF(nbsteps, myfigsize = 16, Init = []):
    # Init is a sequence 1, 0, 0, 1 ...
    # The 1 is for C, the 0 for D : we start from this point.
    #MaximaC = maxima(" matrix ([1, 0, 0], [0, 1, 1], [1, 0, 2])")
    #MaximaD = maxima(" matrix ([0, 1, 1], [1, 0, 0], [1, 0, 2])")
    MC = matrix(QQ, [[1, 0, 0], [0, 1, 1], [1, 0, 2]])
    MD = matrix(QQ, [[0, 1, 1], [1, 0, 0], [1, 0, 2]])
    InitMat = matrix(QQ, [[1, 0, 0], [0, 1, 0], [0, 0, 1]])
    for k in range(len(Init)):
        if Init[k] == 1:
            InitMat *= MC
        else:
            InitMat *= MD
    ToRange = cartesian_product([False, True] for k in range(nbsteps+1))
    ListPoints = []
    for seq in ToRange:
        #Let us read the term ... reversewise :)
        if seq[0]:
            MM = MC
        else:
            MM = MD
        #print(seq)
        for k in range(1, nbsteps):
            if seq[k]:
                MM *= MC
            else:
                MM *= MD
        bigpoint = getPerronFrobeniusEigenvector(InitMat*MM)
        ListPoints.append([bigpoint[0]/bigpoint[2], bigpoint[1]/bigpoint[2]])

    ## Add two main points
    for special in ([1,0,1], [0,1,1]):
        aux = InitMat*vector(QQ, special)
        ListPoints.append([aux[0]/aux[2], aux[1]/aux[2]])
    ListPoints = sorted(ListPoints, key = lambda x: x[0])
    
    Tag = (r'The curve $\mathcal{}$ from words of length {}').format({'C'}, nbsteps)
    ToPlot = list_plot(ListPoints, frame = True, color = 'green',
                       plotjoined = True, figsize = myfigsize,
                       axes_labels = ['$u$ axis','$v$ axis'],
                       legend_label = Tag)
    ToPlot += list_plot(ListPoints, color = 'royalblue',
                       plotjoined = False, figsize = myfigsize)
    return([ListPoints, ToPlot])

def amplify(ListPoints):
    def amplifier (x):
        return([x[0], x[1]])
    ListPoints = list(map(amplifier, ListPoints))
    ToPlot = list_plot(ListPoints, frame = True, color = 'lightgreen',
                       plotjoined = True, figsize = myfigsize,
                       legend_label = Tag)
    ToPlot += list_plot(ListPoints, color = 'royalblue',
                        plotjoined = False, figsize = myfigsize)

############ Convex Hull ###################################""

def getDomainCS(S, nbsteps, myfigsize = 6, Init = []):
    ListPoints = plotCS(S, nbsteps, myfigsize, Init)[0]
    ListPoints = list(map(lambda x: [float(x[0]), float(x[1])], ListPoints))
    CHull = Polyhedron(vertices = ListPoints)
    pointstoplot = cyclic_sort_vertices_2d(CHull.vertices());
    # Close it:
    pointstoplot.append(pointstoplot[0])
    return([ListPoints, pointstoplot, CHull])

from sage.geometry import all

def rationalCHull(S, nbsteps):
    ListPoints = plotCS(S, nbsteps, 4, [])[0]
    ListPoints = reduce(ListPoints)
    lcmdenum = lcm([ lcm(denominator(p[0]), denominator(p[1])) for p in ListPoints])
    ListPoints = [[p[0]*lcmdenum, p[1]*lcmdenum] for p in ListPoints]
    polytt = LatticePolytope(ListPoints)
    pp = LatticePolytope(ListPoints).polyhedron().vertices()
    ListPointSCHull = [[p[0]/lcmdenum, p[1]/lcmdenum] for p in pp]
    return(ListPointSCHull)

def rationalCHullW(nbsteps):
    ListPointCHullW = rationalCHull([[0,1],[89/280, 89/280]], nbsteps)
    return(ListPointCHullW)


def plotDomainCS(S, nbsteps, WithCCurve = True, myfigsize = 6,
                 Init = [], mycolor = 'lightgreen'):
    [ListPoints, pointstoplot, CHull] = getDomainCS(S, nbsteps, myfigsize, Init)
    #print(list(map(lambda v:v.vector(), pointstoplot)))
    ToPlot = polygon(pointstoplot, frame = True, color = mycolor,
                     axes_labels = ['$u$ axis','$v$ axis'],
                     figsize = myfigsize)
    #print(S)
    ToPlot += list_plot(ListPoints, color = 'red', #size = 25, marker="*",
                        plotjoined = True, figsize = myfigsize)
    if not WithCCurve:
        pointstoplot = list(map(lambda v:v.vector(), pointstoplot))
        pointstoplot = list(map(lambda v:[v[0],v[1]], pointstoplot))
        # There is some loss of precision when computing the hull.
        def tt(p, alist):
            aux = list(map(lambda v: abs(v[0]-p[0])+abs(v[1]-p[1]), alist))
            mymin = 1
            for ll in aux:
                if ll < mymin:
                    mymin = ll
            return(mymin)
        #print(ListPoints)
        #print(pointstoplot)
        UnusedPoints = [p for p in ListPoints if tt(p, pointstoplot) > 10^(-9)]
        #print(UnusedPoints)
        ListPoints = [p for p in ListPoints if p not in UnusedPoints]
        ToPlot += list_plot(ListPoints, color = 'royalblue', size = 20, 
                            plotjoined = False, figsize = myfigsize)
        ToPlot += list_plot(UnusedPoints, color = 'red', size = 25, marker="*",
                            plotjoined = False, figsize = myfigsize, zorder=3)
    else:
        ToPlot += list_plot(ListPoints, color = 'royalblue', size = 20, 
                            plotjoined = False, figsize = myfigsize)
        ToPlot += list_plot(ListPoints, color = 'red', #size = 25, marker="*",
                            plotjoined = True, figsize = myfigsize)
    return(ToPlot)

def plotSimpleDomainCS(S, nbsteps, myfigsize = 6, mycolor = 'lightgreen'):
    [ListPoints, pointstoplot, CHull] = getDomainCS(S, nbsteps, myfigsize, [])
    #print(list(map(lambda v:v.vector(), pointstoplot)))
    ToPlot = polygon(pointstoplot, frame = True, color = mycolor,
                     axes_labels = ['$u$ axis','$v$ axis'],
                     figsize = myfigsize)
    return(ToPlot)

from numpy import polyfit

def plotSimpleDomainCSFit(S, nbsteps, myfigsize = 6, mycolor = 'lightgreen'):
    [ListPoints, pointstoplot, CHull] = getDomainCS(S, nbsteps, myfigsize, [])
    #print(list(map(lambda v:v.vector(), pointstoplot)))
    ToPlot = polygon(pointstoplot, frame = True, color = mycolor,
                     axes_labels = ['$u$ axis','$v$ axis'],
                     figsize = myfigsize)
    pp = polyfit([p[0] for p in pointstoplot],
                 [p[1]+.001 for p in pointstoplot], 20)
    def fpp(x):
        aux = 0
        for k in range(len(pp)):
            aux += pp[k]*x^(len(pp)-1-k)
        return(aux)
    ToPlot += plot(fpp, (0,1), color='red')
    return(ToPlot)

def plotDomainC(nbsteps, WithCCurve = True, myfigsize = 6, Init = []):
    return(plotDomainCS([[0,1]], nbsteps, WithCCurve, myfigsize, Init))

def plotDomainCW(nbsteps, WithCCurve = True, myfigsize = 6, Init = []):
    return(plotDomainCS([[0,1], [89/280, 89/280]], nbsteps, WithCCurve, myfigsize, Init))

def plotDomainCWFit(nbsteps, WithCCurve = True, myfigsize = 6, Init = []):
    return(plotSimpleDomainCSFit([[0,1], [89/280, 89/280]], nbsteps, myfigsize))

def containDomainC(nbsteps, apoint):
    [ListPoints, pointstoplot, CHull] = getDomainC(nbsteps, 6, [])
    return(CHull.contains(apoint))

################  Comparison with Model ###############

def mytheta (u):
    if u == 0:
        return(1)
    return(1 - u*log(1 + 1/u)/log(2))

TheModel = [[0, 1], [89/1387, 1018/1387],
            [89/649, 369/649], [369/1667, 738/1667],
            [89/280, 89/280], [738/1667, 369/1667],
            [369/649, 89/649],
            [1018/1387, 89/1387], [1, 0]]

def finiteModel(u):
    # min ( theta_0, theta_1)
    if u == 0:
        return(1)
    for k in range(1, len(TheModel) +1):
        beg = TheModel[k-1]
        end = TheModel[k]
        if u >= beg[0]:
            if u <= end[0]:
                val = beg[1]*(end[0]-u) + end[1]*(u-beg[0])
                val = val / (end[0] - beg[0])
                return(min(val, mytheta(u)))

def plotUpper(myfigsize = 16, mycolor = 'red'):
    return(plot(mytheta, (0, 1), color = mycolor))

def plotfiniteModel(myfigsize = 16, mycolor = 'red'):
    return(plot(finiteModel, (0, 1), color = mycolor))

def compareModelCurve(nbsteps, myfigsize = 6):
    ListPoints = plotC(nbsteps, myfigsize, [], False)[0]
    CompeteListPoints = list(map(lambda x: (x[0], mytheta(x[0])-x[1]), ListPoints))
    tell = 0.03
    Ref = line([(1/3, -0.002), (1/3, tell)], linestyle='-.', color='red')
    Ref += line([(-0.002, 0.011), (1, 0.011)], linestyle='-.', color='red')
    tag = text((r'$0.011$'), (0.04, 0.012))
    tag += text((r'$\frac{1}{3}$'), (1/3+0.015, -0.001))

    return(list_plot(CompeteListPoints, plotjoined = True, figsize = myfigsize,
                     axes_labels = ['$u$ axis',''], color = 'green') + Ref + tag)

def compareModelConvHullS(S, nbsteps, myfigsize = 6, Init = [],
                          mycolor='royalblue', mythickness = 1):
    [ListPoints, pointstoplot, CHull] = getDomainCS(S, nbsteps, myfigsize, Init)
    pointstoplot.sort(key = lambda x: x[0])
    Distance = 1/2^(nbsteps+4)
    CompeteListPoints = list([[pointstoplot[0][0],
                               mytheta(pointstoplot[0][0])-pointstoplot[0][1]]])
    PreviousPoint = pointstoplot[0]
    for k in range(1, len(pointstoplot)):
        Ici = pointstoplot[k]
        idx = 1
        while (PreviousPoint[0] + idx*Distance) < Ici[0]:
            myx = PreviousPoint[0] + idx*Distance
            myy = PreviousPoint[1]
            myy += idx*Distance*(Ici[1]-PreviousPoint[1])/(Ici[0]-PreviousPoint[0])
            CompeteListPoints.append([myx, mytheta(myx)-myy])
            idx += 1
        CompeteListPoints.append([Ici[0], mytheta(Ici[0])-Ici[1]])
        PreviousPoint = Ici

    print("Using " + str(len(CompeteListPoints))+ " plotting points")
    CompeteListPoints = list(map(lambda x: [x[0]/2, x[1]/2], CompeteListPoints))

    tell = 0.016
    Ref = line([(1/6, -0.002), (1/6, tell)], linestyle='-.', color='red')
    tag = text((r'$\frac{1}{6}$'), (1/6+0.0075, -0.001))

    Ref += line([(-0.002, 0.0055), (1/2, 0.0055)], linestyle='-.', color='red')
    tag += text((r'$0.0055$'), (0.031, 0.006))
    #Ref += line([(-0.002, 0.0154), (1/2, 0.0154)], linestyle='-.', color='red')
    #tag += text((r'$0.0154$'), (0.028, 0.0148))


    return(list_plot(CompeteListPoints, plotjoined = True, figsize = myfigsize,
                     axes_labels = ['$\kappa$ axis','$\lambda$ axis'],
                     color = mycolor, thickness = mythickness) + Ref + tag)

def compareFiniteModelConvHullS(S, nbsteps, myfigsize = 6, Init = [],
                                mycolor='royalblue', mythickness = 1):
    [ListPoints, pointstoplot, CHull] = getDomainCS(S, nbsteps, myfigsize, Init)
    pointstoplot.sort(key = lambda x: x[0])
    Distance = 1/2^(nbsteps+4)
    CompeteListPoints = list([[pointstoplot[0][0],
                               finiteModel(pointstoplot[0][0])-pointstoplot[0][1]]])
    PreviousPoint = pointstoplot[0]
    for k in range(1, len(pointstoplot)):
        Ici = pointstoplot[k]
        idx = 1
        while (PreviousPoint[0] + idx*Distance) < Ici[0]:
            myx = PreviousPoint[0] + idx*Distance
            myy = PreviousPoint[1]
            myy += idx*Distance*(Ici[1]-PreviousPoint[1])/(Ici[0]-PreviousPoint[0])
            CompeteListPoints.append([myx, finiteModel(myx)-myy])
            idx += 1
        CompeteListPoints.append([Ici[0], finiteModel(Ici[0])-Ici[1]])
        PreviousPoint = Ici

    print("Using " + str(len(CompeteListPoints))+ " plotting points")
    CompeteListPoints = list(map(lambda x: [x[0]/2, x[1]/2], CompeteListPoints))

    tell = 0.005
    Ref = line([(1/6, -0.001), (1/6, tell)], linestyle='-.', color='red')
    tag = text((r'$\frac{1}{6}$'), (1/6+0.0085, -0.0004))

    Ref += line([(-0.002, 0.00385), (1/2, 0.00385)], linestyle='-.', color='red')
    tag += text((r'$0.00385$'), (0.031, 0.0040))
    Ref += line([(-0.002, 0.0023), (1/2, 0.0023)], linestyle='-.', color='red')
    tag += text((r'$0.0023$'), (0.031, 0.0026))
    #Ref += line([(-0.002, 0.0154), (1/2, 0.0154)], linestyle='-.', color='red')
    #tag += text((r'$0.0154$'), (0.028, 0.0148))

    return(list_plot(CompeteListPoints, plotjoined = True, figsize = myfigsize,
                     axes_labels = ['$\kappa$ axis',''],
                     color = mycolor, thickness = mythickness) + Ref + tag)

def compareModelConvHull(nbsteps, myfigsize = 6, Init = []):
    return(compareModelConvHullS([[0,1]], nbsteps, myfigsize,
                                 Init, 'royalblue', 0.6))

def compareModelConvHullW(nbsteps, myfigsize = 6, Init = []):
    return(compareModelConvHullS([[0,1], [89/280, 89/280]], nbsteps, myfigsize,
                                 Init, 'red', 0.6))

def compareFiniteModelConvHullW(nbsteps, myfigsize = 6, Init = []):
    return(compareFiniteModelConvHullS([[0,1], [89/280, 89/280]], nbsteps, myfigsize,
                                       Init, 'royalblue', 0.6))

#pp = (89/649, 369/649)
#for k in range(1, 10):
#    pp = fC(pp)
#    print(pp, " ", containDomainC(10, pp))

#Global plot:
# plotC(14)[1]
# Zoom:
# plotC(14, 16, [1, 0, 1, 0])[1]

# TP = plotC(8, 6, [])[1]
# TP += plotUpper(6)
# show(TP)
