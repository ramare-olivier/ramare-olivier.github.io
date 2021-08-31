## Needs sage version at least 8.6
##################################################
#####  Computing some structural invariants ######
##################################################

def SubgroupGenerated(n, q):
    return set(n^j % q for j in xrange(1, euler_phi(q)+1))

def GetLatticeInvariantClasses(q):
    ## First get the subgroups: 
    theSGList = []
    for n in filter(lambda w: gcd(w,q)==1, xrange(1, q)):
        mysub = SubgroupGenerated(n, q)
        #print mysub
        if mysub not in theSGList:
            theSGList.append(mysub)

    theSGList.sort(key=len)
    
    ## Then get the classes: 
    ## Create a copy: 
    theClassList = list(theSGList)

    for n in xrange(0, len(theClassList)):
        aux = theClassList[n]
        for m in xrange(n+1, len(theClassList)):
            if aux.issubset(theSGList[m]):
                theClassList[m] = theClassList[m].difference(aux)
            
    return(theSGList, theClassList)

def GetStructure(q):
    [theSGList, theClassList] = GetLatticeInvariantClasses(q)
    ## size of our matrices:
    nbclasses = len(theSGList)
    ## Getting the exponent:
    theExponent = 1
    for i in xrange(0, nbclasses):
        theExponent = lcm(theExponent, len(theSGList[i]))
    ## Now the character group:
    characterGroup = DirichletGroup(q)
    ## The xrange of invertible classes
    invertibles = [n for  n in filter(lambda w: gcd(w, q)==1, xrange(1, q))]
    if q==1:
        invertibles = [1]
    ## For each cyclic subgroup G0, we need the list of characters for G0perp:
    invariantCharacters = [[ind_e for ind_e in xrange(0, len(characterGroup))
                            if set(theSGList[n]).issubset(characterGroup[ind_e].kernel())]
                           for n in xrange(0, nbclasses)]
    ##
    return(theSGList, theClassList, nbclasses, theExponent,
           euler_phi(q), characterGroup, invertibles, invariantCharacters)

##################################################
########  A formula for |theClassList| ###########
##################################################

def Rho(q,d):
    ff = factor(q)
    res = 1
    for pf in ff:
        if pf[0] == 2:
            if (pf[1] >1) and (d%2==0):
                res *= 2*gcd(d, 2^(pf[1]-2))
        else:
            res *= gcd(d, pf[0]^(pf[1]-1)*(pf[0]-1))
    return res

def Auxf (d, qstar):
    ff = factor(d)
    res = 1
    for pf in ff:
        if pf[0] == 2:
            if qstar%2 == 0:
                res *= 1/2
        else:
            if qstar%pf[0] == 0:
                res *= (pf[0]-1)^2/pf[0]/(pf[0]-2)
            else:
                res *= (pf[0]-1)/(pf[0]-2)
    return res
                
def CardClassList (q):
    ff = factor(q)
    res = 0
    phiq = euler_phi(q)
    for d in divisors(phiq):
        if d%2 == 0:
           res +=  Rho(q,d)/euler_phi(d)*Auxf(d, phiq/d)
           #print d, res
    res *= prod((p-2)/(p-1) for p in prime_divisors(phiq) if p != 2)
    return res
    

##################################################
########  M1Inverse ##############################
##################################################

def GetM1Inverse(q, structure):
    """ structure is the output of GetStructure(q) """
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure
    M1Inverse = matrix(nbclasses)
    for i in xrange(0, nbclasses):
        for j in xrange(0, nbclasses):
            # print( M1Inverse[i,j])
            if theSGList[j].issubset(theSGList[i]):
                M1Inverse[i, j] = moebius(len(theSGList[i])/len(theSGList[j]))
                M1Inverse[i, j] = M1Inverse[i, j]*len(theSGList[j])

    return(M1Inverse/phiq)

# GetM1Inverse(7,GetStructure(7))

##################################################
#######  Computing Nds ###########################
##################################################

def GetNd(d, q, structure):
    """ structure is the output of GetStructure(q) """
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure
    Nd = matrix(nbclasses)
    for i in xrange(0, nbclasses):
        # B0 = theSGList[i]
        for j in xrange(0, nbclasses):
            # B1 =  theSGList[j]
            Nd[i, j] = 0
            for k in xrange(0, nbclasses):
                if theSGList[k].issubset(theSGList[i]):
                    if d*len( theSGList[k].intersection(theSGList[j])) == len(theSGList[j]):
                        Nd[i, j] += moebius(len(theSGList[i])/len(theSGList[k]))
    return Nd
                        
# GetNd(2,7,GetStructure(7))

def GetNds(q, structure):
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure
    # Don't forget to remove the divisor 1 !
    Nds = { d:GetNd(d, q, structure) for d in divisors(theExponent)[1:] }
    # That's an association list (dictionary in python parlance)
    # and you get the element linked with d by calling Nds[d]
    return Nds

#GetNds(7,GetStructure(7))

##################################################
#######  Computing Gamma  ########################
##################################################

def GetHurwitzvalues(q, t, structure, s):
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure
    return [hurwitz_zeta(t*s, a/q) for a in invertibles]

#R=RealIntervalField(200)
#[R(u) for u in GetHurwitzvalues(30,2,GetStructure(30),1)]

def GetLvalues(q, t, structure, s, bigP, CField):
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure

    hurwitzvalues = GetHurwitzvalues(Integer(q), t, structure, s)
    hurwitzvaluesCF = [CField(v) for v in hurwitzvalues]
    return [sum([e(invertibles[ind_invert])*hurwitzvaluesCF[ind_invert]
                 for ind_invert in xrange(0, phiq)])/q^(s*t)
            *prod([CField(1-e(p)/p^(s*t)) for p in filter(lambda w: (w in Primes()), xrange(2, bigP))])
            for e in characterGroup]

#C=ComplexIntervalField(200)
#strut = GetStructure(30)
#[C(u) for u in GetLvalues(30 ,1 ,strut,2, 200)]
def checkGetLvalues(q, s, bigP, C):
    structure = GetStructure(q)
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure

    GL = GetLvalues(q ,1 ,structure ,s , bigP, C)
    return [C(u) for u in [GL[index]
                           /prod([1-characterGroup[index](p)/p^s
                                  for p in filter(lambda w: (w in Primes()), xrange(2, bigP))])
                           for index in xrange(0, phiq)]]

#C=ComplexIntervalField(200)
#checkGetLvalues(30, 2, 200, C)
            
def GetGamma(q, t, structure, s, bigP, prec, CField):
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure
    ## 
    if bigP^(-s*t) < 2^(-prec-10):
        one = RealIntervalField(prec+10)(1-2^(-prec-9), 1+2^(-prec-9))
        Lvalues = [one for e in characterGroup]
    else:
        Lvalues = GetLvalues(q, t, structure, s, bigP, CField)
    return vector([log(CField(prod([Lvalues[ind_e]
                                    for ind_e in invariantCharacters[ind_G0]])).real())
                   for ind_G0 in xrange(0, nbclasses)])

# [real(C(u)) for u in GetGamma(30,1,GetStructure(30),2, 200, C)]

################################################
############  Witt Decomposition  ##############
################################################

def GetVectorsF(coeffsF, howmany):
    Ai = coeffsF + ((howmany-len(coeffsF))*[0])
    sF = [0 for i in xrange(0,howmany)]
    sF[0] = len(coeffsF)-1 ## = deg F
    for k in xrange(1, howmany):
        sF[k] = -k*Ai[k]-add(Ai[i]*sF[k-i] for i in xrange(1, k))
    return sF

def GetVectorbF(coeffsF, howmany):
    bF = [0 for i in xrange(0,howmany)] ## bf[0] is not used
    sF = GetVectorsF(coeffsF, howmany)
    for k in xrange(1, howmany):
        bF[k] = add(moebius(k/d)*sF[d] for d in divisors(k))/k
    return bF

# GetVectorbF([1, -4, 4, 2, -4, 1], 11)
# [0, 4, 2, 2, 2, 3, 1, 0, -8, -22, -53]

from sage.rings.polynomial.complex_roots import complex_roots

def GetBeta(F):
    myroots = complex_roots(F)
    return max(1, max([1/abs(c[0]) for c in myroots]))

def GetBetaRough(coeffsF):
    return max(1, max([abs(c) for c in coeffsF]))

################################################
############  Checking Engines  ################
################################################

def GetVsChecker(q, s, borne = 10000):
    ### Computes an approximate value of the list (zeta(s; q, A))
    ### for A in the lattice-invariant classes.
    structure = GetStructure(q)
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure
    Vsapprox = [1/prod([1.0-1/p^s
                        for p in filter(lambda w: (w in Primes()) and (w%q in theClassList[i]),
                                        xrange(2, borne))])
                for i in xrange(0, nbclasses)]

    for i in xrange(0, nbclasses):
            print "-------------------"
            print "For p mod ", q, " in ",  theClassList[i]
            print "the product of 1/(1-p^{-", s, "}) is about", Vsapprox[i]

################################################
############  Formatting outputs  ##############
################################################

def LaTeXForNumber(w, howmany, nbblockstocut):
    # w is a real number with a (short) integer part and a floating point
    # Retuns a character string int(w).digits where digits concerns the first
    # howmany decimals, separated every 5 of them by '\,' et every block of nbblockstocut
    # on a different line. '\cdots' ends the string.
    thelen = 5
    listchar = list(str(w))
    begpos = listchar.index('.') + 1
    listchar.insert(begpos, '&')

    if len(listchar) > howmany + begpos:
        listchar = listchar[0:howmany + begpos +1]

    n = thelen + begpos + 1
    while n < len(listchar):
        if (n - begpos)/(thelen +1) % nbblockstocut == 0:
            listchar.insert(n, '\n')
        else:
            listchar.insert(n, '\\,')
        n += 1 + thelen

    listchar.append('\cdots')
    return ''.join(listchar)

#LaTeXForNumber(22.01234567812345, 100, 8)
# 
################################################
############  Main Engines  #####################
################################################

import sys
from timeit import default_timer as timer

def GetVs(q, s, nbdecimals, bigP = 100, Verbose = 1, WithLaTeX = 0):
    ### Computes an approximate value of the list (zeta(s; q, A))
    ### for A in the lattice-invariant classes.
    ### We compute directly what happens for primes < bigP.
    ### Don't try me and avoid to select a prime number for bigP.
    start = timer()
    ## Compute the structural invariants:
    if Verbose >= 1:
        sys.stdout.write("Computing the structural invariants ... ")
    ##
    structure = GetStructure(q)
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure

    Nds = GetNds(q , structure)
    M1Inverse = GetM1Inverse(q, structure)
    ##
    if Verbose >= 1:
        print(" done.")
    ## Initial computations:
    if Verbose >= 1:
        sys.stdout.write("Computing the finite product for p < " + str(bigP) + " ... ")
    ##
    prec = ceil(nbdecimals * log(10)/log(2)+10)
    R = RealIntervalField( prec)
    eulerProdIni = [R(1/prod([R(1-1/p^s)
                              for p in filter(lambda w: (w in Primes())
                                              and (w%q in theClassList[i]), xrange(2, bigP))]))
                    for i in xrange(0, nbclasses)]
    #print eulerProdIni
    #print [R(eulerProdIni[i]).lower() for i in xrange(0, nbclasses)] 
    if Verbose >= 1:
        print(" done.")
    #############
    ## Getting r:
    if Verbose >= 1:
        sys.stdout.write("Computing r ... ")
    ##
    cte = nbclasses*len(divisors(theExponent))/2
    r = 2
    while (float(log((1+(r-1)/nbclasses)/2)+(r-1)*log(cte)+log(1+bigP/(2^r*s-1))-s*2^r*log(bigP)
                 + nbdecimals*log(10)) > 0):
        r = r+1
    logerr = R((1+(r-1)/nbclasses)/2*cte^(r-1)*(1+bigP/(2^r*s-1))/bigP^(s*2^r))
    C = ComplexIntervalField( prec )
    #R = RealIntervalField( prec)
    ##
    if Verbose >= 1:
        print "done: we use r = ", r, "."
    ##############
    ## Build the set of indices di:
    if Verbose >= 1:
        sys.stdout.write("Building indices ... ")
    ##
    drange = divisors(theExponent)[1:]
    myindices = [] # Careful! v = 0 is missing!
    for v in xrange(1, r):
        myindices += [w for w in Tuples(drange, v)
                      if prod([w[i] for i in xrange(0, v)]) <= 2^r]
    ##
    if Verbose >= 1:
        print "done: there are ", len(myindices)+1, "summands."
    # print myindices
    ## Giving some informations:
    count = 0
    tellwhen = 20
    ## Storing intermediate results:
    computedGammas = {}
    ## This is a dictionary d:GetGamma(q, d, structure, s, bigP, prec, C)
    computedProductNdsM1Inverse = {(d,):Nds[d]*M1Inverse for d in drange}
    ## This is a dictionary (d1,d2, .., dv):Nds[d1]*Nds[d2]*...*Nds[dv]
    ## Careful! The one element list is (d,) and not (d)
    ## Using the formula. We start with v = 0
    Vsapprox = M1Inverse*GetGamma(q, 1, structure, s, bigP, prec, C)
    for indices in myindices:
        dd = prod(indices)
        if computedGammas.has_key(dd) == False:
            computedGammas[dd] = GetGamma(q, dd, structure, s, bigP, prec, C)
        aux = computedGammas[dd]
        # The next works because the indices are ordered with increasing length.
        if len(indices) > 1:
            Naux = Nds[indices[0]]*computedProductNdsM1Inverse[tuple(indices[1:])]
            computedProductNdsM1Inverse[tuple(indices)] = Naux
        else:
            Naux = computedProductNdsM1Inverse[tuple(indices)]
        Vsapprox = Vsapprox + (-1)^len(indices)*Naux*aux/dd
        #print max([Vsapprox[i].upper()-Vsapprox[i].lower() for i in xrange(0, nbclasses)])
        count += 1
        if (count % tellwhen == 0) and (Verbose >= 2):
            print "   After  ", count, " summands:"
            print Vsapprox

    ## We now have to get the Euler products:
    eulerProds = [[R(eulerProdIni[i]*exp(Vsapprox[i]-logerr)).lower(),
                   R(eulerProdIni[i]*exp(Vsapprox[i]+logerr)).upper()]
                  for i in xrange(0, nbclasses)]
    ##
    if Verbose >= 1:
        for i in xrange(0, nbclasses):
            nbdigits = -floor(log(eulerProds[i][1]-eulerProds[i][0])/log(10))
            print "-------------------"
            print "For p mod ", q, " in ",  theClassList[i]
            print "the product of 1/(1-p^{-", s, "}) is between"
            print eulerProds[i][0]
            print "and"
            print eulerProds[i][1]
            if WithLaTeX == 1:
                print "LaTeX format:"
                howmany = min(nbdecimals, nbdigits)
                print LaTeXForNumber(eulerProds[i][0], howmany, 10)
            print "(", nbdigits, " decimal digits)"
    ##

    end = timer()
    ##
    if Verbose >= 1:
        print "Time taken: ", end - start, "seconds."
    # print myindices
    if Verbose == -1:
        return([bigP, phiq, len(myindices)+1, nbclasses, r, end-start])
    else:
        return theClassList, eulerProds

##############################################################

def GetEulerProds(q, F, G, nbdecimals, bigP = 100, Verbose = 1, WithLaTeX = 0):
    ### Computes an approximate value of the list ()
    ### for A in the lattice-invariant classes.
    ### Careful! bigP may increase in the proof!
    ### We compute directly what happens for primes < bigP.
    start = timer()
    ## Compute the structural invariants:
    if Verbose >= 1:
        sys.stdout.write("Computing the structural invariants ... ")
    ##
    structure = GetStructure(q)
    [theSGList, theClassList, nbclasses, theExponent,
     phiq, characterGroup, invertibles, invariantCharacters] = structure

    Nds = GetNds(q , structure)
    M1Inverse = GetM1Inverse(q, structure)
    ##
    if Verbose >= 1:
        print(" done.")
    #############
    ZX = PolynomialRing(ZZ,x)
    F = ZX(F)
    G = ZX(G)
    ## Get mybeta and bigP:
    mybeta = max(2, GetBeta(F), GetBeta(G))
    ###########"
    if Verbose >= 1:
        print "We have mybeta  = ", mybeta
    #############
    bigP = max(bigP, 2*mybeta)
    ## Initial computations:
    if Verbose >= 1:
        sys.stdout.write("Computing the finite products for p < " + str(bigP) + " ... ")
    ##
    prec = ceil(nbdecimals * log(10)/log(2) + 10)
    R = RealIntervalField( prec)
    ## Please, no empty products ! This gives an error, it is enough to increase bigP
    eulerProdIni = [prod([R(F(1/p)/G(1/p))
                          for p in filter(lambda w: (w in Primes())
                                          and (w%q in theClassList[i]), xrange(2, bigP))])
                    for i in xrange(0, nbclasses)]
    ##
    if Verbose >= 1:
        print(" done.")
    #############
    ## Getting r:
    if Verbose >= 1:
        sys.stdout.write("Computing r, bigJ, b_F, b_G ... ")
    ##
    cte = nbclasses*len(divisors(theExponent))/2
    r = 2
    while (float(log((1+(r-1)/nbclasses)/4*mybeta^2)+(r-1)*log(cte)+log(1+bigP/2^r)
                 -2^(r+1)*log(bigP)-log(1-mybeta/bigP^(4))
                 + log(2*max(F.degree(), G.degree())) + (nbdecimals+1)*log(10)) > 0):
        r = r + 1
        
    bigJ = 2
    while (float((bigJ+1)*log(mybeta)-bigJ*log(bigP)-log(bigJ+1)
                 + log(1/bigP + 1/bigJ) - log(1-mybeta/bigP)
                 + log(2*max(F.degree(), G.degree())) + (nbdecimals+1)*log(10)) > 0):
        bigJ = bigJ + 1

    bF = GetVectorbF(F.list(), bigJ +1)
    bG = GetVectorbF(G.list(), bigJ +1)
    logerr = R(2*max(F.degree(), G.degree())
               *((1+(r-1)/nbclasses)/4*cte^(r-1)*(1+bigP/2^r)*mybeta^2/bigP^(2^(r+1))
                 /(1-mybeta/bigP^4)
                 + (mybeta/bigP)^bigJ*mybeta/(bigJ+1)/(1-mybeta/bigP)*(1/bigP+1/bigJ)))
    # bG(j)-bG(j) can be very large!
    prec = prec + ceil(float(bigJ*log(mybeta)/log(2))) 
    C = ComplexIntervalField( prec)
    ##
    if Verbose >= 1:
        print "done: we use r = ", r, " bigJ = ", bigJ, " and working prec = ", prec, "."
    ##############
    ## Build the set of indices di:
    if Verbose >= 1:
        sys.stdout.write("Building indices ... ")
    ##
    drange = divisors(theExponent)[1:]
    myindices = [] # Careful! v = 0 is missing!
    for v in xrange(1, r):
        myindices += [w for w in Tuples(drange, v)
                      if prod([w[i] for i in xrange(0, v)]) <= 2^r]
    ##
    if Verbose >= 1:
        print "done: there are ", len(myindices)+1, "summands."
    # print myindices
    ## Giving some informations:
    count = 0
    tellwhen = 20
    ## Storing intermediate results:
    computedGammas = {}
                  ## This is a dictionary d:GetGamma(q, d, structure, 1, bigP, prec, C)
    computedProductNdsM1Inverse = {(d,):Nds[d]*M1Inverse for d in drange}
    ## This is a dictionary (d1,d2, .., dv):Nds[d1]*Nds[d2]*...*Nds[dv]
    ## Careful! The one element list is (d,) and not (d)

    logZsapprox = vector([0 for u in theClassList])
    ## The main loop in j:
    for j in xrange(2, bigJ+1):
        ## We compute log zeta_P(j; q, A)
        ## Using the formula. We start with v = 0
        Vsapprox = M1Inverse*GetGamma(q, j, structure, 1, bigP, prec, C)
        for indices in myindices:
            dd = prod(indices)
            if computedGammas.has_key(j*dd) == False:
                computedGammas[j*dd] = GetGamma(q, j*dd, structure, 1, bigP, prec, C)
            aux = computedGammas[j*dd]
            #print aux
            # The next works because the indices are ordered with increasing length.
            if len(indices) > 1:
                Naux = Nds[indices[0]]*computedProductNdsM1Inverse[tuple(indices[1:])]
                computedProductNdsM1Inverse[tuple(indices)] = Naux
            else:
                Naux = computedProductNdsM1Inverse[tuple(indices)]
            Vsapprox = Vsapprox + (-1)^len(indices)*Naux*aux/dd
            count += 1
            if (count % tellwhen == 0) and (Verbose >= 2):
                print "   After  ", count, " summands:"
                print Vsapprox
        ## End of the loop in indices
        logZsapprox = logZsapprox + (bG[j] - bF[j])*Vsapprox
    ## End of the main loop in j
    print logZsapprox
    
    ## We now have to get the Euler products:
    eulerProds = [[R(eulerProdIni[i]*exp(logZsapprox[i]-logerr)).lower(),
                   R(eulerProdIni[i]*exp(logZsapprox[i]+logerr)).upper()]
                  for i in xrange(0, nbclasses)]
    ##
    if Verbose >=1:
        for i in xrange(0, nbclasses):
            nbdigits = -floor(log(abs(eulerProds[i][1]-eulerProds[i][0]))/log(10))
            print "-------------------"
            print "For p mod ", q, " in ",  theClassList[i]
            print "For F(x) = ", F
            print "and G(x) = ", G
            print "the product of F(1/p)/G(1/p) is between"
            print eulerProds[i][0]
            print "and"
            print eulerProds[i][1]
            if WithLaTeX == 1:
                print "LaTeX format:"
                howmany = min(nbdecimals, nbdigits)
                print LaTeXForNumber(eulerProds[i][0], howmany, 10)
            print "(", nbdigits, " decimal digits)"
    ##

    end = timer()
    ##
    if Verbose >= 1:
        print "Time taken: ", end - start, "seconds."
    # print myindices
    
    return theClassList, eulerProds
    
# GetEulerProds(3, 1, 1-x^2, 100)
#####################################################
#####################################################
#########  Table Makers #############################
#####################################################
#####################################################

def TablePerformance(minq, maxq, prec=100, bigP=300):
    res = {}
    for q in xrange(minq, maxq+1):
        if (q%2 == 0) and (q%4 !=0):
            pass
        else:
            sys.stdout.write(str(q) + " ")
            sys.stdout.flush()
            aux = GetVs(q, 2, prec, bigP, -1)
            print aux
            res[q] = aux
            
    for q in xrange(minq, maxq +1):
        if res.has_key(q):
            str_res = str(q)
            for i in xrange(1, 5):
                str_res = str_res + "& " + str(res[q][i])
                
            str_res = str_res + "& " + str(ceil(res[q][5]*1000)) + " \\\\"
            print str_res
    return


