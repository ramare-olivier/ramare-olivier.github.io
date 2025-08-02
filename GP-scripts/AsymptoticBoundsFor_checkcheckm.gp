
/*
 Change the two lines postfaced by  / * HERE!!! * /
 to modify the model agains which we compare
*/

{getboundsteraux(zmin:small, zmax:small, tellwhen:small = 200000, \\ zmin >= 1
                 WhereToWrite = "Moebius-check-check-3-bis.txt") =
   local(maxi, maxiall, valuesmu:vecsmall, gotit:vecsmall, thepoint:real,
         valuesm:vec, valuesmlog:vec, valuesmloglog:vec, vallog:vec, aux:real, storeres:vec);
   \\zmin = max( 2, floor(zmin));
   write(WhereToWrite, "Nouvelle execution de getboundsteraux( ", zmin, ", ", zmax, ", ", tellwhen, " )"); 
   gettime();
   write(WhereToWrite, "With checkcheck(x) = \sum_{n\le x} mu(n)\log^2 (x/n)/n, and");
   write(WhereToWrite, "     S0(x) = |checkcheck(x)| / modelmult(x), where the chosen");
   write(WhereToWrite, "     multiplicative model is defined in the script, we have:");
   /* Precomputations des valeurs de mu(n): */
   valuesmu = vectorsmall(zmax+1, m, 1);   \\ zmax+1 !!
   gotit = vectorsmall(zmax+1, m, 1);      \\ zmax+1 !!
   forprime (p:small = 2, floor(sqrt(zmax+1.0)),
             for(k:int = 1, floor((zmax+1.0)/p), 
                valuesmu[k*p] *= -1;
                gotit[k*p] *= p));
   forprime (p:small = 2, floor(sqrt(zmax+1.0)),
             for(k:small = 1, floor((zmax+1.0)/p^2),
                valuesmu[k*p^2] = 0;
                gotit[k*p^2] = 0));

   write(WhereToWrite, "Fin des precalculs (Moebius values, I), I: ", gettime()); \\ print(valuesmu);

   vallog = vector(zmax+1);
   for(n:small = 2, zmax+1,
      vallog[n] = log(n);
      if(gotit[n] < n, 
         valuesmu[n] = -valuesmu[n],));

   write(WhereToWrite, "Fin des precalculs (Log values + end Moebius values), II: ", gettime());

   valuesm = vector(zmax+1);
   valuesmlog = vector(zmax+1);
   valuesmloglog = vector(zmax+1);
   valuesm[1] = 1.0;
   valuesmlog[1] = 0.0;  
   valuesmloglog[1] = 0.0;  
   aux = 1.0; 
   for(n:small = 2, zmax+1,
      /*  BEWARE !!!!!!!!!!!!!! Choose the second line to go fast ! */
      \\valuesm[n] = valuesm[n-1]+valuesmu[n]/n;
      valuesm[n] = (valuesm[n-1]+0.0)+valuesmu[n]/n;
      valuesmlog[n] = (valuesmlog[n-1]+0.0)+valuesmu[n]*vallog[n]/n;
      valuesmloglog[n] = (valuesmloglog[n-1]+0.0)+valuesmu[n]*vallog[n]^2/n;
   );

   write(WhereToWrite, "Fin des precalculs (m, mlog and mlogsquare values), III: ", gettime()); 
      \\ print(valuesm);

   maxi = 0.0;
   maxiall = 0.0;
   storeres = vector(ceil(zmax/tellwhen), m, 0.0);
   for(n:small = zmin, zmax,  \\ zmax and not zmax+1
      if((valuesmlog[n]+1)/valuesm[n] < vallog[n],
         thepoint = vallog[n],
         if((valuesmlog[n]+1) /valuesm[n]> vallog[n+1],
            thepoint = vallog[n+1],
            thepoint = (valuesmlog[n]+1)/valuesm[n]));
      aux = abs(valuesm[n]*thepoint^2-2*valuesmlog[n]*thepoint+valuesmloglog[n]
               -2*thepoint+2*Euler);
      aux = max(aux,
                abs(valuesm[n]*vallog[n]^2-2*valuesmlog[n]*vallog[n]+valuesmloglog[n]
               -2*vallog[n]+2*Euler));
      aux = max(aux,
                abs(valuesm[n]*vallog[n+1]^2-2*valuesmlog[n]*vallog[n+1]+valuesmloglog[n]
               -2*vallog[n+1]+2*Euler));
      \\print("val = ", valuesm[n]*thepoint^2-2*valuesmlog[n]*thepoint+valuesmloglog[n]);
      \\print("--> " 2*thepoint-2*Euler);
      \\print("m     : ", valuesm[n]);
      \\print("mLog  : ", valuesmlog[n]);
      \\print("mLog^2: ", valuesmloglog[n]);

      aux = aux*(1+log(n+1.0):real);                /* HERE!!! */ 
      \\aux = aux*(sqrt(n+0.0):real);               
      \\aux = aux*vallog[n]^2/(0.00965*vallog[n]-0.00818);   
      \\aux = aux*vallog[n]*103;              
      maxi = max(maxi, aux);
         \\ Just now the following line is empty !! 
         \\ Put maxi = max(maxi, aux); *after* if you want this line
         \\ To have any effect !!
      if(aux > maxi,
         maxi = aux; write(WhereToWrite, "Local champion: ", n, " value = ", aux),);
      
      if(n%tellwhen == 0, 
         storeres[n/tellwhen] = maxi;
         maxiall = max(maxiall, maxi);
         write(WhereToWrite, "Upto ", n, " : S0(x) <= ", maxi, " [En tout : ",maxiall,"]");
         maxi = 0.0,));
   write(WhereToWrite, "Bound for ",zmin, " <= z <= ", zmax); \\print(valuesmu);
   write(WhereToWrite, "Fin: ",gettime());
   return([maxiall, maxi]);
}



/* getboundsteraux(10000, 200000, 10000) */
