/*
 Change the two lines postfaced by  / * HERE!!! * /
 to change the model agains which we compare
*/

{getboundsmstar(zmin:small, zmax:small, tellwhen:small = 200000, \\ zmin >= 1
           WhereToWrite = "Moebius-4.txt") =
   local(res:real = 0.0, maxi, maxiall, valuesmu:vecsmall, gotit:vecsmall,
         valuesm:vec, aux1:real, aux2:real, aux:real, storeres:vec);
   \\zmin = max( 2, floor(zmin));
   write(WhereToWrite, "Nouvelle execution de getboundsmstar( ", zmin, ", ", zmax, ", ", tellwhen, " )"); 
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

   valuesm = vector(zmax);
   valuesm[1] = 1.0;
   aux = 1.0; 
   for(n:small = 2, zmax,
      /*  BEWARE !!!!!!!!!!!!!! Choose the second line to go fast ! */
      \\valuesm[n] = valuesm[n-1]+valuesmu[n]/n;
      valuesm[n] = (valuesm[n-1]+0.0)+valuesmu[n]/n;
   );

   write(WhereToWrite, "Fin des precalculs, III: ", gettime()); \\ print(valuesm);

   maxi = 0.0;
   maxiall = 0.0;
   storeres = vector(ceil(zmax/tellwhen), m, 0.0);

   for(n:small = zmin, zmax,
      res = 0.0;
      for(w:small = 1, floor(sqrt(n+0.0)),
         if(w^2 <= n-1,
            aux1 = abs(valuesm[floor((n-1.0)/w^2)]),
            aux1 = 0.0);
         aux2 = abs(valuesm[floor((n+0.0)/w^2)]);
         res = res + max(aux1, aux2)/w^2);
      \\aux = res*((n-1.0)^(1/2)/sqrt(1+log(n-1.0)):real);   
      aux = res*(1+log(n+0.0):real);                              /* HERE!!! */
            \\aux = res*((n-1.0)^(1/2));
      maxi = max(maxi, aux);
         \\ Just now the following line is empty !! 
         \\ Put maxi = max(maxi, aux); *after* if you want this line
         \\ To have any effect !!
      if(aux > maxi,
         maxi = aux; write(WhereToWrite, "Local champion: ", n, " value = ", aux),);
      
      if(n%tellwhen == 0, 
         storeres[n/tellwhen] = maxi;
         maxiall = max(maxiall, maxi);
         write(WhereToWrite, "Upto ", n, " : ", maxi, " [En tout : ",maxiall,"]");
         maxi = 0.0,));

\\   maxi = max(maxi, abs(res)*((zmax+0.0)^(1/2)/sqrt(1+log(zmax)):real));   
   maxi = max(maxi, abs(res)*(1+log(zmax):real));                                 /* HERE!!! */
   \\maxi = max(maxi, abs(res)*((zmax+0.0)^(1/2)));
   maxiall = max(maxiall, maxi);
   write(WhereToWrite, "Bound for ", zmin, " <= z <= ", zmax); \\print(valuesmu);
   write(WhereToWrite, "Fin: ", gettime());
   return([maxiall, maxi]);
}

