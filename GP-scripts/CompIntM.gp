
{getintM(zmax:small) =
   my(res1:real, res2:real, maxi, maxiall, valuesmu:vecsmall, gotit:vecsmall,valM:real);
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


   valM = 1;
   res1 = 1.0; /* integrale de 1 a 2 ! */
   res2 = 1.0; /* integrale de 1 a 2 ! */
   for(n:small = 2, zmax,
      valM += valuesmu[n];
      if(valM>0, res1 += valM, res2 += -valM);
   );

   return([res1, res2]);
}
