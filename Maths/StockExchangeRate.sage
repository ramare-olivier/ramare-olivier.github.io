def plotPublis(adict):
    # ln(alist) > 2
    smoothinglen = 3 # Moyenne sur 3 ans
    smoothedlist = [(1994 + k,
                     sum([(smoothinglen-abs(n))*adict[1994 + min(max(0, k-n), len(adict)-1)]
                          for n in [-smoothinglen .. smoothinglen]])
                     /smoothinglen^2)
                    for k in [0..len(adict)-1]]
    list_plot(smoothedlist, plotjoined = True).show()
    return(smoothedlist)

Mypubs = {
    1994 : 1,
    1995 : 1,
    1996 : 2,
    1997 : 0,
    1998 : 4,
    1999 : 1,
    2000 : 0,
    2001 : 2,
    2002 : 1,
    2003 : 3,
    2004 : 1,
    2005 : 1,
    2006 : 0,
    2007 : 2,
    2008 : 2,
    2009 : 2,
    2010 : 4,
    2011 : 0,
    2012 : 2,
    2013 : 5,
    2014 : 3,
    2015 : 1,
    2016 : 4,
    2017 : 4,
    2018 : 6,
    2019 : 3,
    2020 : 2,
    2021 : 4,
    2022 : 2,
    2023 : 1}

    

# load("StockExchangeRate.sage")
