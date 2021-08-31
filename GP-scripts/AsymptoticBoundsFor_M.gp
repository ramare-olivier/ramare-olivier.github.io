/*
 Change the two lines postfaced by  / * HERE!!! * /
 to change the model agains which we compare
*/


{getboundsM(zmin:small, zmax:small, tellwhen:small = 200000, \\ zmin >= 1
           WhereToWrite = "Moebius-M-1.txt") =
   local(res:real = 0.0, maxi, maxiall, valuesmu:vecsmall, gotit:vecsmall,
         valuesM:vec, aux1:real, aux2:real, aux:real, storeres:vec);
   \\zmin = max( 2, floor(zmin));
   write(WhereToWrite, "Nouvelle execution de getbounds( ", zmin, ", ", zmax, ", ", tellwhen, " )"); 
   gettime();
   /* Precomputations des valeurs de mu(n): */
   valuesmu = vectorsmall(zmax, m, 1);
   gotit = vectorsmall(zmax, m, 1);
   forprime (p:small = 2, floor(sqrt(zmax+0.0)),
             for(k:int = 1, floor((zmax+0.0)/p), 
                valuesmu[k*p] *= -1;
                gotit[k*p] *= p));
   forprime (p:small = 2, floor(sqrt(zmax+0.0)),
             for(k:small = 1, floor((zmax+0.0)/p^2),
                valuesmu[k*p^2] = 0;
                gotit[k*p^2] = 0));

   write(WhereToWrite, "Fin des precalculs, I: ", gettime()); \\ print(valuesmu);

   for(n:small = 2, zmax,
      if(gotit[n] < n, 
         valuesmu[n] = -valuesmu[n],));

   write(WhereToWrite, "Fin des precalculs, II: ", gettime()); \\ print(valuesm);

   valuesM = vector(zmax);
   valuesM[1] = 1.0;
   aux = 1.0; 
   for(n:small = 2, zmax,
      /*  BEWARE !!!!!!!!!!!!!! Choose the second line to go fast ! */
      \\valuesM[n] = valuesM[n-1]+valuesmu[n];
      valuesM[n] = (valuesM[n-1]+0.0)+valuesmu[n];
   );

   write(WhereToWrite, "Fin des precalculs, III: ", gettime()); \\ print(valuesm);

   maxi = 0.0;
   maxiall = 0.0;

   for(n:small = zmin, zmax,
      aux = max(abs(valuesM[n]*(1+log(n+0.0))/n), abs(valuesM[n]*(1+log(n+1.0))/(n+1))); /* HERE!!! */
      maxi = max(maxi, aux);
      
      if(n%tellwhen == 0, 
         maxiall = max(maxiall, maxi);
         write(WhereToWrite, "Upto ", n, " : ", maxi, " [En tout : ",maxiall,"]");
         maxi = 0.0,));

   maxiall = max(maxiall, maxi);
   write(WhereToWrite, "Bound for ", zmin, " <= z <= ", zmax); \\print(valuesmu);
   write(WhereToWrite, "Fin: ", gettime());
   return([maxiall, maxi]);
}

