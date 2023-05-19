/*
 Change the two lines postfaced by  / * HERE!!! * /
 to change the model agains which we compare
*/

{dcheckm(logpoint, valm, valmlog, valmloglog)=
   return(abs(valm*logpoint^2-2*valmlog*logpoint+valmloglog));
}

{getboundscheckcheckmstar(zmin:small, zmax:small, tellwhen:small = 200000, \\ zmin >= 1
           WhereToWrite = "Moebius-1-checkcheckm.txt") =
   local(res:real = 0.0, maxi, maxiall, valuesmu:vecsmall, gotit:vecsmall, qui:int, quiplus:int,
         valuesm:vec, valuesmlog:vec, valuesmloglog:vec, 
         vallog:vec, aux1:real, aux2:real, aux:real, storeres:vec, thepoint:real);
   \\zmin = max( 2, floor(zmin));
   write(WhereToWrite, "Nouvelle execution de getbounds( ", zmin, ", ", zmax, ", ", tellwhen, " )"); 
   gettime();
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

   write(WhereToWrite, "Fin des precalculs, I: ", gettime()); \\ print(valuesmu);

   vallog = vector(zmax+1);
   for(n:small = 2, zmax+1,
      vallog[n] = log(n);
      if(gotit[n] < n, 
         valuesmu[n] = -valuesmu[n],));

   write(WhereToWrite, "Fin des precalculs, II: ", gettime()); \\ print(valuesm);

   valuesm = vector(zmax+1);
   valuesmlog = vector(zmax+1);
   valuesmloglog = vector(zmax+1);
   valuesm[1] = 1.0;
   valuesmlog[1] = 1.0;   \\ !!!!!!!!!!!!!!! We start 1 !!  
   valuesmloglog[1] = 2*Euler; \\ !!!!!!!!!!!!!!! We start at 2*Euler !!  
   aux = 1.0; 
   for(n:small = 2, zmax+1,
      /*  BEWARE !!!!!!!!!!!!!! Choose the second line to go fast ! */
      \\valuesm[n] = valuesm[n-1]+valuesmu[n]/n;
      valuesm[n] = (valuesm[n-1]+0.0)+valuesmu[n]/n;
      valuesmlog[n] = (valuesmlog[n-1]+0.0)+valuesmu[n]*vallog[n]/n;
      valuesmloglog[n] = (valuesmloglog[n-1]+0.0)+valuesmu[n]*vallog[n]^2/n;
   );

   write(WhereToWrite, "Fin des precalculs, III: ", gettime()); \\ print(valuesm);

   maxi = 0.0;
   maxiall = 0.0;
   storeres = vector(ceil(zmax/tellwhen), m, 0.0);
   for(n:small = zmin, zmax,  \\ zmax and not zmax+1
      res = 0.0;
      for(w:small = 1, floor(sqrt(n+1.0)),
         if(w^2 <= n,
            qui = floor((n+0.0)/w^2);
            quiplus = floor((n+1.0)/w^2);

            if(quiplus > qui,
               thepoint = (valuesmlog[qui])/valuesm[qui];
               aux1 = max(dcheckm(vallog[n]-2*vallog[w], 
                                  valuesm[qui], valuesmlog[qui], valuesmloglog[qui]),
                          dcheckm(vallog[quiplus],
                                  valuesm[quiplus], valuesmlog[quiplus], valuesmloglog[quiplus]));
               if((thepoint > vallog[n]-2*vallog[w])&&(thepoint < vallog[quiplus]),
                  aux1 = max(aux1,
                             dcheckm(thepoint,
                                     valuesm[qui], valuesmlog[qui], valuesmloglog[qui])),);
               aux2 = dcheckm(vallog[n+1]-2*vallog[w],
                              valuesm[quiplus], valuesmlog[quiplus], valuesmloglog[quiplus]);

               thepoint = (valuesmlog[quiplus])/valuesm[quiplus];
               if((thepoint >  vallog[quiplus])&&(thepoint < vallog[n+1]-2*vallog[w]),
                  aux1 = max(aux1,
                             dcheckm(thepoint,
                                     valuesm[quiplus], valuesmlog[quiplus], valuesmloglog[quiplus])),);
               res = res + max(aux1, aux2)/w^2,

               aux1 = max(dcheckm(vallog[n]-2*vallog[w],
                                  valuesm[qui], valuesmlog[qui], valuesmloglog[qui]),
                          dcheckm(vallog[n+1]-2*vallog[w],
                                  valuesm[qui], valuesmlog[qui], valuesmloglog[qui]));
               thepoint = (valuesmlog[qui])/valuesm[qui];
               if((thepoint > vallog[n]-2*vallog[w])&&(thepoint < vallog[n+1]-2*vallog[w]),
                  aux1 = max(aux1,
                             dcheckm(thepoint,
                                     valuesm[qui], valuesmlog[qui], valuesmloglog[qui])),);

               res = res + aux1/w^2),
            /* w^2 > n */
            ));

      \\aux = res*((n+0.0)^(1/2)/sqrt(1+(log(n+0.0))):real);  
      aux = res*((n+0.0)^(1/2));       /* HERE!!! */        
      \\aux = res*(log(n+0.0):real);
      \\aux = res*((n-1.0)^(1/2));
      maxi = max(maxi, aux);
         \\ Just now the following line has empty effect!! 
         \\ Put maxi = max(maxi, aux); *after* if you want this line
         \\ To have any effect !!
      if(aux > maxi,
         maxi = aux; write(WhereToWrite, "Local champion: ", n, " value = ", aux),);
      
      if(n%tellwhen == 0, 
         storeres[n/tellwhen] = maxi;
         maxiall = max(maxiall, maxi);
         write(WhereToWrite, "Upto ", n, " : ", maxi, " [En tout : ",maxiall,"]");
         maxi = 0.0,));

   \\maxi = max(maxi, abs(res)*((zmax+0.0)^(1/2)/(1+log(zmax)):real));   
  \\ maxi = max(maxi, abs(res)*(log(zmax):real));                        
   \\maxi = max(maxi, abs(res)*((zmax+0.0)^(1/2)));
   maxiall = max(maxiall, maxi);
   write(WhereToWrite, "Bound for ", zmin, " <= z <= ", zmax); \\print(valuesmu);
   write(WhereToWrite, "Fin: ", gettime());
   return([maxiall, maxi]);
}


/*
allocatemem(7900000000)
getboundscheckcheckmstar(600000, 12000000, 200000)
*/
