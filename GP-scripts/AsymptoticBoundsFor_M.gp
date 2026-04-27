{g(n) = return(moebius(n)/n)}

/* Choose your model and put it last: */
/* This model has to be monotonous between two integers, 
   from zmin onwards, defined in function getboundsbis.
   A non-increasing model is ok. */
{modelmult(z) = 1/sqrt(z);}
{modelmult(z) = (0.0144*log(z)-0.1)/((log(z))^2);}
{modelmult(z) = 1/(69*(log(z)));}
{modelmult(z) = 0.0144/(log(z));}
{modelmult(z) = 1/(40*log(z));}
{modelmult(z) = 1/(45*log(z));}
{modelmult(z) = 1/(65*log(z));}
{modelmult(z) = 1/(25*log(z));}
{modelmult(z) = 1/(20*log(z));}
{modelmult(z) = 1/(12*log(z));}
{modelmult(z) = 1/10;}
{modelmult(z) = 1/20;}
{modelmult(z) = 1/100;}
{modelmult(z) = 1/(1+log(z));}
{modelmult(z) = (0.0144*log(z)-0.118)/((log(z))^2);}

{getboundsbis(zmin, zmax, tellwhen = 500000, WhereToWrite = "Moebius-11.txt") =
   /* Results printed in file Moebius-11.txt. */
   local(res = 0.0, m, maxi, maxiall);
   zmin = max( 0, floor(zmin));
   zmax = ceil(zmax);
   write(WhereToWrite, "With m(x) = \sum_{n\le x} mu(n)/n, and");
   write(WhereToWrite, "     S0(x) = |m(x)| / modelmult(x), we have:");
   for(n = 1, zmin, res += g(n));
   maxi = abs(res)/modelmult(zmin);
   maxiall = maxi;
   for(n = zmin+1, zmax,
      m = modelmult(n);
      maxi = max(maxi, abs(res)/m); 
      res += g(n);
      maxi = max(maxi, abs(res)/m);
      if(n%tellwhen==0, 
         maxiall = max(maxiall, maxi);
         write(WhereToWrite, "Up to ", n, " : S0(x) \le ", maxi, " [In all: ", maxiall, "]");
         maxi = 0,));
    maxi = max(maxi, abs(res)/modelmult(zmax));
   maxiall = max(maxiall, maxi);
   write(WhereToWrite, "Bound for ",zmin, " <= z <= ", zmax);
   return([maxiall, maxi]);
}

\\getboundsbis(100, 10^11);

\\ getbounds(10, 3000000)
\\ default(primelimit, 2*10^9)
