/*    -*-MuPAD-*-

     Title:     DiCh.mu
     Created:   Wed Aug  9 22:18:37 2000
     Author:    Olivier Ramare
                <ramare@pepe.ramare.fr>

 Description: Computation of dichromatic polynomials. See Dichromate.help.

 Exported Libraries:

*/

DiChromateq:    // Points  
DiChromatev:    // Aretes  
DiChromatex:
DiChromatey:
DiChromateqvxyDefault:=[hold(q),hold(v),hold(x),hold(y)]:

/*-------------------------------------------------------------------*/
/*     Le Cas general                                                */
/*------------------------------------------------------------------ */
/*
 Description: 
   For us, a graph is given by a list of vertices and a list of
 of edges. Our graphs will not be directed.
 Here is an example: Go:=[{1,2,3,4},[[1,2],[2,3],[3,4],[4,1]]]:
*/

Dichromate::IsGraph:=
  proc(G)
  begin
    bool(nops(G)=2 and type(G[1])=DOM_SET and type(G[2])=DOM_LIST
         and _and(op(map(G[2],fun(nops(args(1))=2))))
         and ({op(map(G[2],op))} minus G[1] = {}))
  end_proc:

Dichromate::NeighbourTab:=
  proc(G)
  /* On calcule la table des voisins, avec repetitions. */
  /* G de type [{1,2,3,4},[[1,2],[2,3],[3,4],[4,1]]]    */
    local aux, tab;
  begin
    tab:=table(aux=[]$aux in G[1]);
    for aux in G[2] do
      tab[aux[1]]:=append(tab[aux[1]],aux[2]);
      if aux[1]<>aux[2] then
        tab[aux[2]]:=append(tab[aux[2]],aux[1]);
      end_if;
    end_for;
    return(tab);
  end_proc:

Dichromate::NormalForm:=
  proc(G)
    /* Il s'agit simplement d'appeler les points 1, 2 etc.   */
    /* C'est essentiel si vous voulez que les tables de remember */
    /* soient efficaces !!                                       */
    local ind, x, tab, tob, Gordered, table_incidence, m, pt,cmp, cnt;
  begin
    table_incidence:=Dichromate::NeighbourTab(G);
    Gordered:=[op(G[1])];
    tab:=table(_seqin(pt = nops(table_incidence), pt, Gordered));
    cnt:=fun(tab[args(1)]^2);
    tob:=table(_seqin(pt = max(0, op(map(table_incidence[pt], cnt))),
                      pt, Gordered));
    cmp:=proc(a, b)
    begin
      if tab[a] < tab[b] then
        return(bool(FALSE))
      else 
        if tab[a] = tab[b] then 
          return(bool(tob[a] < tob[b]))
        else 
          return(TRUE)
        end_if
      end_if
    end_proc; 
    Gordered:=sort(Gordered, cmp);
    tab:=table(Gordered[ind] = [ind, ind]$hold(ind) = 1..nops(G[1])); 
    G[1]:={ind $ hold(ind) = 1..nops(G[1])}; 
    for ind from 1 to nops(G[2]) do 
      if tab[G[2][ind][1]][1] < tab[G[2][ind][2]][1] then 
        G[2][ind]:=[tab[G[2][ind][1]][2], tab[G[2][ind][2]][2]]
      else 
        G[2][ind]:=[tab[G[2][ind][2]][2], tab[G[2][ind][1]][2]]
      end_if
    end_for; 
    return(G)
  end_proc:

Dichromate::ConnectedCompo:=
  proc(G)
  /* Le resultat est une liste de graphes : les composantes connexes. */
    local list_graph, base, aux, NextCompo;
  begin
    NextCompo:= proc(G, list_v)
                  local aux, new_set, new_edges, old_set;
                begin
                  new_set:=list_v; 
                  repeat 
                    old_set:=new_set; 
                    for aux in G[2] do 
                      if nops(new_set intersect {op(aux)}) = 1 then 
                        new_set:=new_set union {op(aux)}
                      end_if
                    end_for
                  until nops(new_set) = nops(old_set) end_repeat; 
                  new_edges:=[]; 
                  for aux in G[2] do 
                    if nops(new_set intersect {op(aux)})=nops({op(aux)}) then 
                      new_edges:=append(new_edges, aux)
                    end_if
                  end_for;
                  return([new_set, new_edges])
                end_proc;

    list_graph:=[];
    while (nops(G[1])>0) do
      list_graph:=linsert(list_graph,[NextCompo(G,{op(G[1],1)})],0);
      G[1]:=G[1] minus list_graph[1][1];
      base:=[];
      for aux in G[2] do
	if contains(list_graph[1][2],aux)=0
	  then base:=append(base,aux);
	end_if;
      end_for;
      G[2]:=base;
    end_while;
    return(list_graph)
  end_proc:

Dichromate::DiChromatic:=
  proc(G)
    local pol, fct, mult, table_incidence, aux, n;
  begin
    if testargs() then
      /* args ! */
      fct:=fun([type(args(1)),nops(args(1))]);
      if args(0)<>1 then
        error("One argument expected (a graph). See file Dichromate.help.")
      elif not (type(G)=DOM_LIST and nops(G)=2) then
        error("Argument expected should be a list of two elements.
               See file Dichromate.help.")
      elif not ({op(map(G[2],fct))} = {[DOM_LIST,2]}
                and type(G[1])=DOM_SET) then
        error("List of edges is improper. See file Dichromate.help.")
      elif not ({op(map(G[2],op))} minus G[1] = {}) then
        error("Some edge goes out of the graph. See file Dichromate.help.")
      end_if;
    end_if;
    
    userinfo(0,"Preparing ground.");
    table_incidence:=Dichromate::NeighbourTab(G);
   
    /* We first remove loops    : */
    userinfo(2,"Removing loops.");
    mult:=1;
    for aux in G[1] do
      for n from nops(table_incidence[aux]) downto 1 do
        if table_incidence[aux][n]=aux
           then mult:=mult*hold(B);
                table_incidence[aux][n]:=NIL;
	end_if;
      end_for;
    end_for;
    /* Our graph doesn't have any loops left. */
    /* Repare G[2]:                           */
    for n from nops(G[2]) downto 1 do
      if G[2][n][1]=G[2][n][2]
	then G[2][n]:=NIL;
      end_if;
    end_for;

    pol:=Dichromate::innerchromatic(G);
    userinfo(2,"Computations are ended. Checking the result.");
    if ({type(DiChromateq),type(DiChromatev)}<>{DOM_IDENT}
        or
        2^(nops(G[2]))=subs(pol,{DiChromatev=1,DiChromateq=1,hold(B)=2}))
      then userinfo(1,"Checked or uncheckable.")
    else
      userinfo(0,"Something went wrong...",G)
    end_if;
    /* Create Dichromate::DDD for Simpler */
    if type(DiChromatev)=DOM_IDENT then
      if type(DiChromateq)=DOM_IDENT then
        Dichromate::DDD:=Poly([hold(X)],Poly([DiChromateq,DiChromatev]))
      else
        Dichromate::DDD:=Poly([hold(X)],Poly([DiChromatev]))
      end_if;
    else
      if type(DiChromateq)=DOM_IDENT then
        Dichromate::DDD:=Poly([hold(X)],Poly([DiChromateq]))
      else
        Dichromate::DDD:=Poly([hold(X)])
      end_if;
    end_if;
    return(Dichromate::Simpler(subs(pol*mult,hold(B)=DiChromatev+1)))
  end_proc:

Dichromate::innerchromatic:=
  proc(G)
  /* Interne a Dichromate::DiChromatic.                                        */
  /* S'utilise en duo avec Dichromate::simplechromatic.                        */
  /* En fait Dichromate::simplechromatic est la partie de Dichromate::innerchromatic */
  /* qui s'occupe des composantes connexes, mais comme il                */
  /* peut y avoir deconnection en cours de route,                        */
  /* Dichromate::simplechromatic appelle Dichromate::innerchromatic.     */
  /* G may not be connected, but it will not have any loops.             */
    local list_components, compo, pol_aux;
  begin
    userinfo(2,"Splitting Into Connected Components ...");
    list_components:=Dichromate::ConnectedCompo(G);
    pol_aux:=1;
    userinfo(2,list_components);
    userinfo(2,"Done. There are ".nops(list_components)." Components.");
    for compo in list_components do
      pol_aux:=Dichromate::simplechromatic(Dichromate::NormalForm(compo))*pol_aux
    end_for;
    return(pol_aux)
  end_proc:

Dichromate::simplechromatic:=
  proc(G)
  /* Interne a Dichromate::DiChromatic.                */
  /* S'utilise en duo avec Dichromate::innerchromatic  */
  /* G will be connected and with no loops.            */
  /* It may have some multiple edges.                  */
    option remember;        /* Huge time saver ! */
    local glue, mult, alonep, aux, glued_G, n, lonely_ones,
          table_incidence, pt, m;
  begin
    glue:= proc(G,v1,v2)
             local aux, new_G;
           begin
             new_G:=[{},[]];
             for aux in G[2] do
               if (aux[1]=v2) then aux:=[v1,aux[2]] end_if;
               if (aux[2]=v2) then aux:=[aux[1],v1] end_if;
               new_G[2]:=append(new_G[2],aux);
             end_for;
             new_G[1]:=G[1] minus {v2};
             return(new_G)
           end_proc;

    if (nops(G[1])=1)
      /* Simple Point or n-loop */
      then return(DiChromateq*hold(B)^(nops(G[2])));
    elif nops(G[1])=2
      then return(DiChromateq*DiChromatev+DiChromateq^2
                  +DiChromatev*DiChromateq
                  *(hold(B)^(nops(G[2]))-hold(B))/(hold(B)-1))
    elif (nops(G[1])=3 and nops(G[2])=2)
      then return(2*DiChromateq^2*DiChromatev+DiChromateq^3
                  +DiChromateq*DiChromatev^2)
    else
      table_incidence:=Dichromate::NeighbourTab(G);
      
      /* We then remove loose ends : */
      lonely_ones:={};
      for aux in G[1] do
        alonep:=TRUE;
	for n from 1 to nops(table_incidence[aux])-1 do
	  alonep:=bool(table_incidence[aux][n]=table_incidence[aux][n+1]);
	  if (not alonep)
	    then break;
	  end_if;
	end_for;
	if alonep
	  then lonely_ones:=lonely_ones union {aux}
	end_if;
      end_for;      /* Two loose ends cannot be linked one with the other */

      G[1]:= G[1] minus lonely_ones;
      mult:=1;
      for aux in lonely_ones do
        mult:=mult*(DiChromateq+DiChromatev
                                *(hold(B)^(nops(table_incidence[aux]))-1)
                                /(hold(B)-1));
      end_for;
      /* Note that table_incidence is wrong now ... */
      /* Repare G[2]: */
      for n from nops(G[2]) downto 1 do
        if contains(lonely_ones,G[2][n][1]) or contains(lonely_ones,G[2][n][2])
	  then G[2][n]:=NIL;
	end_if;
      end_for;
      /* The graph is still connected but may have some loose ends left. */

      if nops(G[2])=0
	then return (expand(mult*DiChromateq^(nops(G[1]))))
      else
	/* We select an edge with a large multiplicity */

    userinfo(2,"Splitting/Gluing with a ".nops(G[1]).":".nops(G[2])." Graph.");
	aux:=G[2][1];
	m:=0;
	for n from nops(G[2]) downto 1 do
	  if G[2][n]=aux
	    then m:=m+1;
	         G[2][n]:=NIL;
	  end_if;
	end_for;
	glued_G:=Dichromate::NormalForm(glue(G,aux[1],aux[2]));
	/* G may not be connected any more but glued_G  */
	/* is connected. None of them contain any loop. */
	return(expand(mult*(Dichromate::innerchromatic(G)
                            +DiChromatev*(hold(B)^m-1)/(hold(B)-1)
                             *Dichromate::simplechromatic(glued_G))));
      end_if;
    end_if;
  end_proc:

Dichromate::RectGraph:=
  proc(r,n)
  /* Le graphe rectangulaire r X n avec rn sommets. */
    local vertices, edges,a, b, Nr, Nn;
  begin
    if r<n
      then Nr:=r; Nn:=n;
    else
      Nr:=n; Nn:=r;
    end_if;
    vertices:={([a,b]$hold(a)=1..Nr)$hold(b)=1..Nn};
    edges:=[];
    for a from 1 to Nr-1 do
      for b from 1 to Nn do
	edges:=append(edges,[[a,b],[a+1,b]]);
      end_for
    end_for;
    for b from 1 to Nn-1 do
      for a from 1 to Nr do
	edges:=append(edges,[[a,b],[a,b+1]]);
      end_for
    end_for;
    return(Dichromate::NormalForm([vertices,edges]))
  end_proc:

/*-------------------------------------------------------------------*/
/*     Le Cas special des rectangles                                 */
/*-------------------------------------------------------------------*/

Dichromate::Mat:=Dom::Matrix(Dom::ExpressionField(normal,iszero)):

Dichromate::MakeAll:=proc(r,RefEdges,linearp)
  /* Construction de la matrice associee a la fonction 1 du graphe :
     Dichromate::One[x,y]=1 si x <= y, et 0 sinon.
     On construit aussi Dichromate::TabChap, Dichromate::Chapeaux, Dichromate::DivChap,
     Dichromate::TabCompo, Dichromate::TabKappa,Dichromate::DDD     */
  local x,y,MdC,dim, IMakeChapeaux, Chaplessp,SortChap,Composantes,Kappa,
        IMakePartitions;
  begin
    IMakeChapeaux:=proc(rr)
    /* Procedure recursive et Dichromate::IMakeChapeaux(4) renvoit la liste des
       Chapeaux de taille 4. Attention !!!! Un chapeau est ici un couple
       [nb_composantes,[liste_des_numeros]], c'est a dire quelque chose
       comme [2,[1,1,2,1]].  */
                     local aux, where, who, new_one, chap, x, joint;
                   begin
                     if rr=1 then
                       return([[1,[1]]])
                     else
                       new_one:=[];
                       aux:=IMakeChapeaux(rr-1);
                       for who from 1 to nops(aux) do
                         chap:=aux[who];
                        /* Either we add a new class: */
                         new_one:=append(new_one,[chap[1]+1,
                                                  append(chap[2],chap[1]+1)]);
                        
                        /* Either join last point to already existing class: */
                         joint:=[];
                         for x from chap[1] downto 1 do
                         /* 'x' ranges the possible equivalence classes */
                           where:=rr-1;
                           while (chap[2][where]>x) do
                                  where:=where-1
                           end_while;
                         /* so 'where' is either the last member of class 'x'
                           either a member of a class imbedding class 'x'   */
                           if (chap[2][where]=x) then
                           /* can jump till 'where' while keeping planarity */
                             joint:=append(joint,x)
                           end_if;
                         end_for;
                         for x in joint do
                           new_one:=append(new_one,[chap[1],append(chap[2],x)])
                         end_for;
                       end_for;
       /* La liste finale est de taille binomial(2*rr+1,rr)/(2*rr+1)*/
                       return(new_one)
                     end_if;
                   end_proc;

    IMakePartitions:=proc(rr)
    /* Procedure recursive et Dichromate::IMakePartitions(4) renvoit la liste des
       Chapeaux de taille 4. Attention !!!! Un chapeau est ici un couple
       [nb_composantes,[liste_des_numeros]], c'est a dire quelque chose
       comme [2,[1,1,2,1]].  */
                     local aux, where, who, new_one, chap, x, joint;
                   begin
                     if rr=1 then
                       return([[1,[1]]])
                     else
                       new_one:=[];
                       aux:=IMakePartitions(rr-1);
                       for who from 1 to nops(aux) do
                         chap:=aux[who];
                        /* Either we add a new class: */
                         new_one:=append(new_one,[chap[1]+1,
                                                  append(chap[2],chap[1]+1)]);
                        
                        /* Either join last point to already existing class: */
                         joint:=[];
                         for x from chap[1] downto 1 do
                         /* 'x' ranges the possible equivalence classes */
                           new_one:=append(new_one,[chap[1],append(chap[2],x)])
                         end_for;
                       end_for;
                       return(new_one)
                     end_if;
                   end_proc;

    Chaplessp:=proc(c1,c2)
     /* Donc le plus petit a r composantes, le plus grand en a 1 */
                 local classe, res, who, debut, friend;
               begin
                 res:=TRUE;
                 for classe from 1 to c1[1] do
                   debut:=classe;
                   while (c1[2][debut]<>classe) do
                          debut:=debut+1
                   end_while;
                   friend:=c2[2][debut];
                   for who from debut+1 to nops(c1[2]) do
                     if c1[2][who]=classe then
                       res:=bool(res and (c2[2][who]=friend))
                     end_if;
                   end_for;
                 end_for;
                 return(res)
               end_proc;

    SortChap:=proc()
    /* Un chapeau a n composantes. Ses diviseurs (i.e. les gens
       qui sont plus petits que lui) ont > n composantes.
       Nous ordonnons alors Chapeaux suivant le nombre de composantes.
       Nous disposons alors de la propriete:
       " Si Chapeau[i] <= Chapeau[j] alors i <= j ". */
                local niveau, who, new_one;
              begin
                new_one:=[];
                for niveau from 1 to nops(Dichromate::Chapeaux[1][2]) do
                  for who from 1 to nops(Dichromate::Chapeaux) do 
                    if Dichromate::Chapeaux[who][1]=niveau then
                      new_one:=append(new_one,Dichromate::Chapeaux[who])
                    end_if;
                  end_for;
                end_for;
                Dichromate::Chapeaux:=new_one;
              end_proc;

    Composantes:=proc(c1)
    /* Une nouvelle facon d'ecrire les chapeaux: la liste des composantes.
       */
                   local  who, new_one;
    /* option remember; Inutile. Nous gérons la table nous meme a l'aide de
       Dichromate::TabCompo ci-dessous. Ceci evite d'avoir a passer le contexte et
       de plus l'argument de Dichromate::TabCompo est uniquement l'indice, ce qui est
       plus leger que de passer le chapeau en entier. */
                 begin
                   new_one:=[[]$c1[1]];
                   for who from 1 to nops(c1[2]) do
                     new_one[c1[2][who]]:=append(new_one[c1[2][who]],who)
                   end_for;
                   return(new_one)
                 end_proc;

    Kappa:=proc(c)
    /* 'c' est un chapeau (i.e. par exemple [2,[1,1,2,1]]).
       Le resultat est un entier >= 0 . */
             local res, who;
           begin
             res:=0;
             
             /* for who from 1 to (nops(c[2]))-1 do
               if c[2][who]=c[2][who+1] then
                 res:=res+1
               end_if;
                 end_for;*/
             for who in RefEdges do
               if c[2][who[1]]=c[2][who[2]] then res:=res+1
               end_if;
             end_for;
             return(res)
           end_proc;

    if linearp then
      Dichromate::Chapeaux:=IMakeChapeaux(r)
    else
      Dichromate::Chapeaux:=IMakePartitions(r)
    end_if;
    dim:=nops(Dichromate::Chapeaux);
    SortChap();
    if type(DiChromatev)=DOM_IDENT then
      if type(DiChromateq)=DOM_IDENT then
        Dichromate::DDD:=Poly([hold(X)],Poly([DiChromateq,DiChromatev]))
      else
        Dichromate::DDD:=Poly([hold(X)],Poly([DiChromatev]))
      end_if;
    else
      if type(DiChromateq)=DOM_IDENT then
        Dichromate::DDD:=Poly([hold(X)],Poly([DiChromateq]))
      else
        Dichromate::DDD:=Poly([hold(X)])
      end_if;
    end_if;
    Dichromate::TabChap:=table(Dichromate::Chapeaux[MdC]=MdC$hold(MdC)=1..dim);
    Dichromate::TabCompo:=[Composantes(Dichromate::Chapeaux[MdC])$hold(MdC)=1..dim];
    Dichromate::TabKappa:=[Kappa(Dichromate::Chapeaux[MdC])$hold(MdC)=1..dim];        
    Dichromate::DivChap:=table(MdC=[]$hold(MdC)=1..dim);
    
    Dichromate::One:=Dichromate::Mat(dim,dim,0):
    for x from 1 to dim do
      for y from 1 to x do
        if Chaplessp(Dichromate::Chapeaux[x],Dichromate::Chapeaux[y]) then
          Dichromate::One[x,y]:=1;
          Dichromate::DivChap[y]:=append(Dichromate::DivChap[y],x)
        end_if;
      end_for;
    end_for;
  end_proc:
 
Dichromate::MakeMoeb:=proc(r)
  option remember;
  begin
    Dichromate::Moeb:=Dichromate::One^(-1)
  end_proc:

Dichromate::MakeMoebBis:=proc(r)
  option remember;
  begin
    Dichromate::Moeb:=linalg::linearSolve(Dichromate::One,
                                    op((Dichromate::One)::dimen(Dichromate::One)),
                                    1,hold(Diagonal))
  end_proc:

/* Bon, pour ce qui de l'ordre des coordonnees...
   Dichromate::One[x,y]  c'est 1_{x <= y} et donc
   Dichromate::Moeb[x,y] c'est mu(x,y).                       */

/*--------------------------------------------------------------------*/

Dichromate::GammaFlat:=proc(indc1,indc2)
  /* indc1 et indc2 sont des indices de chapeaux.
     Le resultat est une expression en DiChromateq et DiChromatev.*/
  local cps, other, res, compo, cp, cps2, cp2;
  begin
    cps:=Dichromate::TabCompo[indc1];
    cps2:=Dichromate::TabCompo[indc2];
    res:=1;
    for cp from 1 to nops(cps) do
      other:=0;
      compo:=cps[cp];
      for cp2 from 1 to nops(cps2) do
        other:=other
               +(DiChromatev+1)^(nops({op(compo)} intersect {op(cps2[cp2])}))-1
      end_for;
      res:=res*(DiChromateq+other)
    end_for;
    return(res)
  end_proc:
           
Dichromate::Coeff:=proc(indc1,indc2)
  /* indc1 et indc2 sont les indices des chapeaux.
     La reponse est ( Dichromate::Chapeaux[indc1] : Dichromate::Chapeaux[indc2] ) .
     Nous ne l'utilisons pas pour la serie, il s'agit du coefficient lorsque
     la matrice est multipliee par Dichromate::One^(-1)...*/
  local who,indc3,res;
  begin
    userinfo(2,"Calcul du coefficient ".indc1." ".indc2);
    res:=0;
    for indc3 in Dichromate::DivChap[indc2] do
      if Dichromate::Moeb[indc3,indc2]<>0 then
        res:=res+Dichromate::Moeb[indc3,indc2]*Dichromate::GammaFlat(indc1,indc3)
                                  *(DiChromatev+1)^(Dichromate::TabKappa[indc3])
      end_if;
    end_for;
    return(simplify(res))
  end_proc:

/*---------------------------------------------------------------------*/
/* Des fonctions de cette region, seule InitialVect est utilisee pour
   le calcul de la serie. Sinon ces fonctions sont utiles pour Pi.     */

Dichromate::InitialVect:=proc(RefEdges)
  local vect, x, r, Part, SousEdges, sous, SubMultiSet, TabEdge;
  begin
    Part:=proc(Edges,r)
            local comp, done, comp_list, e, chap, x;
          begin 
            comp_list:=[];
            for e in Edges do
              done:=FALSE;
              /* Try to add it to some existing component */
              for c from 1 to nops(comp_list) do
                if contains(comp_list[c],e[1])
                   or contains(comp_list[c],e[2]) then
                  comp_list[c]:=comp_list[c] union {op(e)};
                  done:=TRUE;
                  break;
                end_if;
              end_for;
              if not done then
                /* Create a new component */
                comp_list:=append(comp_list,{op(e)})
              end_if;
            end_for;

            /* Remains the isolated guys */
            comp_list:=comp_list.[op(map({x$hold(x)=1..r}
                                         minus _union(op(comp_list)),
                                         fun({args(1)})))];
            /* Careful to the order, for Dichromate::TabChap. */
            comp_list:=sort(comp_list,
                            fun(bool(sort([op(args(1))])[1]
                                     <sort([op(args(2))])[1])));

            /* Now build chap from comp_list: */  
            chap:=[0,[0$r]];
            for c in comp_list do
              chap[1]:=chap[1]+1;
              for x in c do
                chap[2][x]:=chap[1]
              end_for;
            end_for;

            /* Careful to normal form: lowest class number should come first */
            return(chap)
          end_proc;
                  
    r:= nops(Dichromate::Chapeaux[1][2]);
    SousEdges:=[];
    SubMultiSet:=
       map(combinat::powerset({x$hold(x)=1..nops(RefEdges)}),
           fun(map([op(args(1))],fun(RefEdges[args(1)]))));

    for sous in SubMultiSet do
      SousEdges:=append(SousEdges,[nops(sous),Part(sous,r)])
    end_for;
    vect:=Dichromate::Mat(1,nops(Dichromate::Chapeaux),0);
    for x from 1 to nops(SousEdges) do
      vect[1,Dichromate::TabChap[SousEdges[x][2]]]:=
         vect[1,Dichromate::TabChap[SousEdges[x][2]]]+ DiChromatev^(SousEdges[x][1])
    end_for;
    return(vect)
  end_proc:

Dichromate::PsiLocal:=proc(indc)
 local indc2;
 option remember;
 begin
   return(Dichromate::Mat
          (1,nops(Dichromate::Chapeaux),
           [Dichromate::Coeff(indc,indc2)$hold(indc2)=1..nops(Dichromate::Chapeaux)]))
 end_proc:

Dichromate::Pi:=proc(s,RefEdges)
      /* Donc le polynome chromatique d'une grille rXs.
         Il vaut : si DiChromatev=0, DiChromateq^(r*s)
                   si DiChromateq=0, 0
                   si DiChromateq=1, (DiChromatev+1)^(nombres d'aretes) */
 local Psi,vect, wh,r;
 begin
   r:=nops(Dichromate::Chapeaux[1][2]);
   Psi:=fun(_plus(args(1)[wh]*(Dichromate::PsiLocal)(wh)
                  $hold(wh)=1..nops(Dichromate::Chapeaux)));
   vect:=fp::nest(Psi,s-1)(Dichromate::InitialVect(RefEdges));
   return(normal(_plus(vect[wh]*DiChromateq^(Dichromate::Chapeaux[wh][1])
                       $hold(wh)=1..nops(Dichromate::Chapeaux))))
 end_proc:

Dichromate::Simpler:=proc(expre)
  begin
    expre:=op(Dichromate::DDD(numer(expre)),1)/op(Dichromate::DDD(denom(expre)),1);
    if type(DiChromateq)=DOM_IDENT then
      expre:=subs(expre,DiChromateq=DiChromateqvxyDefault[1])
    end_if;
    if type(DiChromatev)=DOM_IDENT then
      expre:=subs(expre,DiChromatev=DiChromateqvxyDefault[2])
    end_if;
    if type(DiChromatex)=DOM_IDENT then
      expre:=subs(expre,DiChromatex=DiChromateqvxyDefault[3])
    end_if;
    if type(DiChromatey)=DOM_IDENT then
      expre:=subs(expre,DiChromatey=DiChromateqvxyDefault[4])
    end_if;
    return(expre)
  end_proc:
           
/*-------------------------------------------------------*/

Dichromate::IGetSeries:=proc(RefEdges)
  local ResolvanteInverse,indc,dim,specoef,linSolve;
  begin
   linSolve:=
     proc(A,B)
     /* Tres fortement inspire de linalg::linSolve/gaussElim/gaussjord */
       local t, i, j, sz, k, ns, s, p, T, c, d, _ns, de, r;
     begin
       if A::hasProp( Cat::SquareMatrixCat ) then
         t:= A::getSuperDomain(A)
       else
         t:= type(A)
       end_if;
       newThis:= t::newThis;
       sz:=A::dimen(A)[1];
       
       userinfo(1,"compute Gauss-Jordan form");
       T:= A::gaussElim(A::concatMatrix( A,t::coerce(B)));
       A:= op(T,1);   ns:= op(T,4);

       d:= A::dimen(A);  r:= d[1];   c:= d[2];
       userinfo(2,"compute row reduced form");
       
       /* ds -- list of [i,j] whereby A[i,j] <> 0 */
       t:= [];  j:= 1;
       for i from 1 to min(c,sz) do
       	if not contains( ns,i ) then
	    t:= append(t,[j,i]);
	    j:= j + 1;
	    if j > r then break end_if
 	end_if
       end_for;
       /* Cette facon de programmer peut sembler bizarre : il aurait suffit
       d'ordonner t de l'autre sens au depart, mais en l'occurence,
       append est une fonction tres efficace. t[1] >= t[2] */
       de:= []; /* sort descending */
       for i from nops(t) downto 1 do
         de:= append( de,t[i])
       end_for;
       /* bon, alors 'de' contient des [j,i] tous les i etant distincts,
       et tous les j etant distincts et decroissants. */

       for t in de do
         A[t[1],sz+1]:= A[t[1],sz+1]/A[t[1],t[2]];
         A[t[1],t[2]]:= 1;
         for i from t[1]-1 downto 1 do
           A[i,sz+1]:= normal(A[i,sz+1] - A[i,t[2]]*A[t[1],sz+1])
         end_for
       end_for;

       /* Nous calculons la solution speciale : */
       s:= newThis(sz,1);

       j:= 1;
       for i from 1 to sz do
         if not contains( ns,i ) then
           s[i,1]:= A[j,sz+1];
           j:= j + 1;
           if j > sz then break # i # end_if
         end_if
       end_for;
       return(s)
     end_proc;

    dim:=nops(Dichromate::Chapeaux);
    specoef:=fun(Dichromate::One[args(2),args(1)]
                 -hold(X)*normal(Dichromate::GammaFlat(args(2),args(1))
                                 *(DiChromatev+1)^(Dichromate::TabKappa[args(1)])));
    ResolvanteInverse:=Dichromate::Mat(dim,dim,specoef);
    userinfo(1,"Calcul de la serie.");
    return(
     normal(
      (Dichromate::Mat(1,dim,[DiChromateq^(Dichromate::Chapeaux[indc][1])
                        $hold(indc)=1..dim])*
       linSolve(ResolvanteInverse,
                linalg::transpose(Dichromate::InitialVect(RefEdges)*Dichromate::One))
       )[1,1]));
  end_proc: 

/*--------------------------------------------------------*/
/* Precomputations:                                       

Dichromate::RectDiChromatic(2,hold(All)):=
(q*v + q^2 + X*q^2*v^2*(-1) + X*q^2*v^3*(-1))
*(X*(q*v*(-3) + q^2*(-1) + v^2*(-4) + v^3*(-1))
  + X^2*(v^4 + v^5 + q*v^3*2 + q*v^4*2 + q^2*v^2 + q^2*v^3) + 1)^(-1):

Dichromate::RectDiChromatic(3,hold(All)):=
(q^3 + q*v^2 + q^2*v*2 + X*q*v^6 + X*q^2*v^4*(-9) + X*q^3*v^3*(-9)
 + X*q^4*v^2*(-2) + X*q^2*v^5*(-7) + X*q^3*v^4*(-6) + X*q^4*v^3*(-1)
 + X*q^2*v^6*(-2) + X*q^3*v^5*(-1) + X^2*q*v^8*(-1) + X^2*q*v^9*(-1)
 + X^2*q^2*v^7*3 + X^2*q^3*v^6*11 + X^2*q^4*v^5*8 + X^2*q^5*v^4
 + X^2*q^2*v^8*5 + X^2*q^3*v^7*17 + X^2*q^4*v^6*12 + X^2*q^5*v^5
 + X^2*q^2*v^9*2 + X^2*q^3*v^8*7 + X^2*q^4*v^7*5 + X^2*q^3*v^9
 + X^2*q^4*v^8 + X^3*q^3*v^9*(-1) + X^3*q^4*v^8*(-2) + X^3*q^5*v^7*(-1)
 + X^3*q^3*v^10*(-3) + X^3*q^4*v^9*(-6) + X^3*q^5*v^8*(-3)
 + X^3*q^3*v^11*(-3) + X^3*q^4*v^10*(-6) + X^3*q^5*v^9*(-3)
 + X^3*q^3*v^12*(-1) + X^3*q^4*v^11*(-2) + X^3*q^5*v^10*(-1))
*(X*q^3*(-1) + X*v^3*(-15) + X*v^4*(-6) + X*v^5*(-1) + X*q*v^2*(-12)
  + X*q^2*v*(-5) + X*q*v^3*(-1) + X^2*v^6*32 + X^2*v^7*30 + X^2*v^8*10
  + X^2*v^9 + X^3*v^9*(-15) + X^3*v^10*(-25) + X^3*v^11*(-12)
  + X^3*v^12*(-2) + X^4*v^12 + X^4*v^13*3 + X^4*v^14*3 + X^4*v^15
  + X^2*q*v^5*62 + X^2*q*v^6*51 + X^2*q*v^7*14 + X^2*q*v^8
  + X^3*q*v^8*(-49) + X^3*q*v^9*(-79) + X^3*q*v^10*(-36)
  + X^3*q*v^11*(-6) + X^4*q*v^11*5 + X^4*q*v^12*15 + X^4*q*v^13*15
  + X^4*q*v^14*5 + X^2*q^2*v^4*43 + X^2*q^3*v^3*15 + X^2*q^4*v^2*2
  + X^2*q^2*v^5*29 + X^2*q^3*v^4*9 + X^2*q^4*v^3 + X^2*q^2*v^6*5
  + X^2*q^3*v^5 + X^3*q^2*v^7*(-62) + X^3*q^3*v^6*(-38) + X^3*q^4*v^5*(-11)
  + X^3*q^5*v^4*(-1) + X^3*q^2*v^8*(-96) + X^3*q^3*v^7*(-56)
  + X^3*q^4*v^6*(-15) + X^3*q^5*v^5*(-1) + X^3*q^2*v^9*(-41)
  + X^3*q^3*v^8*(-22) + X^3*q^4*v^7*(-5) + X^3*q^2*v^10*(-7)
  + X^3*q^3*v^9*(-4) + X^3*q^4*v^8*(-1) + X^4*q^2*v^10*10 + X^4*q^3*v^9*10
  + X^4*q^4*v^8*5 + X^4*q^5*v^7 + X^4*q^2*v^11*30 + X^4*q^3*v^10*30
  + X^4*q^4*v^9*15 + X^4*q^5*v^8*3 + X^4*q^2*v^12*30 + X^4*q^3*v^11*30
  + X^4*q^4*v^10*15 + X^4*q^5*v^9*3 + X^4*q^2*v^13*10 + X^4*q^3*v^12*10
  + X^4*q^4*v^11*5 + X^4*q^5*v^10 + 1)^(-1):                            */

/*-------------------------------------------------------*/

Dichromate::RectDiChromatic:=proc(r,s,Flag)
  /* Flag est optionel */
  /* r is a graph or an integer. s is an integer or 'All'.
     Flag is TRUE or FALSE.                                */
  local aux, WholeSeries, forcep, RefEdges, DefEdges;
  begin
    case args(0)
      of 1 do
        if testargs() and (not (type(r)= DOM_INT or Dichromate::IsGraph(r))) then
          error("[1] Integer argument expected. See file Dichromate.help.")
        else
          if type(r)=DOM_INT then
            aux:=[r,[[aux,aux+1]$hold(aux)=1..(r-1)],hold(All),FALSE];break
          else
            aux:=[nops(r[1]),r[2],hold(All),FALSE];break
          end_if;
        end_if;
      of 2 do
        if type(r)=DOM_INT and type(s)=DOM_INT then
          aux:=[min(r,s),[[aux,aux+1]$hold(aux)=1..(min(r,s)-1)],
                max(r,s),FALSE];
        elif type(r)=DOM_INT and s=hold(All) then
          aux:=[r,[[aux,aux+1]$hold(aux)=1..(r-1)],hold(All),FALSE];
        elif Dichromate::IsGraph(r) and (type(s)=DOM_INT or s=hold(All)) then
          aux:=[nops(r[1]),r[2],s,FALSE];
        else
          error("[2] See file Dichromate.help.")
        end_if;
        break;
      of 3 do
        if type(Flag)=DOM_BOOL then
          if type(r)=DOM_INT and type(s)=DOM_INT then
            aux:=[min(r,s),[[aux,aux+1]$hold(aux)=1..(min(r,s)-1)],
                  max(r,s),Flag];
          elif type(r)=DOM_INT and s=hold(All) then
            aux:=[r,[[aux,aux+1]$hold(aux)=1..(r-1)],hold(All),Flag];
          elif Dichromate::IsGraph(r) and (type(s)=DOM_INT or s=hold(All)) then
            aux:=[nops(r[1]),r[2],s,Flag];
          else
            error("[3] See file Dichromate.help.")
          end_if;
        else
          error("[4] See file Dichromate.help.")
        end_if;
        break;
      otherwise error("[5] Too many arguments !! See file Dichromate.help.")
    end_case;
                
    forcep:=aux[4];/* Utiliser Dichromate::Pi et non la serie */
    r:=aux[1];
    RefEdges:=aux[2];
    s:=aux[3];
    /* Initialisation */
    Dichromate::MakeAll(r,RefEdges,TRUE);
    userinfo(1,"Fin de l'initialisation ... Debut du boulot !");

    if (s<> hold(All)) and (forcep or s<r^2) then
      Dichromate::MakeMoeb(r);
      return(Dichromate::Simpler(Dichromate::Pi(s,RefEdges)))
    else
      userinfo(1,"Computing the whole series...");
      WholeSeries:=Dichromate::IGetSeries(RefEdges);
      if s=hold(All) then
        return(Dichromate::Simpler(WholeSeries))
      else
        return(Dichromate::Simpler(op(taylor(Dom::Expression(WholeSeries),
                                       hold(X),s+1),[4,s])))
      end_if;
    end_if;
  end_proc:

Dichromate::RectTutte:=proc(r,s,Flag)
  local res,k,nv, aux, WholeSeries, forcep, RefEdges, DefEdges,
        oldDiChromateq;
  begin
    case args(0)
      of 1 do
        if testargs() and (not (type(r)= DOM_INT or Dichromate::IsGraph(r))) then
          error("[1] Integer argument expected. See file Dichromate.help.")
        else
          if type(r)=DOM_INT then
            aux:=[r,[[aux,aux+1]$hold(aux)=1..(r-1)],hold(All),FALSE];break
          else
            aux:=[nops(r[1]),r[2],hold(All),FALSE];break
          end_if;
        end_if;
      of 2 do
        if type(r)=DOM_INT and type(s)=DOM_INT then
          aux:=[min(r,s),[[aux,aux+1]$hold(aux)=1..(min(r,s)-1)],
                max(r,s),FALSE];
        elif type(r)=DOM_INT and s=hold(All) then
          aux:=[r,[[aux,aux+1]$hold(aux)=1..(r-1)],hold(All),FALSE];
        elif Dichromate::IsGraph(r) and (type(s)=DOM_INT or s=hold(All)) then
          aux:=[nops(r[1]),r[2],s,FALSE];
        else
          error("[2] See file Dichromate.help.")
        end_if;
        break;
      of 3 do
        if type(Flag)=DOM_BOOL then
          if type(r)=DOM_INT and type(s)=DOM_INT then
            aux:=[min(r,s),[[aux,aux+1]$hold(aux)=1..(min(r,s)-1)],
                  max(r,s),Flag];
          elif type(r)=DOM_INT and s=hold(All) then
            aux:=[r,[[aux,aux+1]$hold(aux)=1..(r-1)],hold(All),Flag];
          elif Dichromate::IsGraph(r) and (type(s)=DOM_INT or s=hold(All)) then
            aux:=[nops(r[1]),r[2],s,Flag];
          else
            error("[3] See file Dichromate.help.")
          end_if;
        else
          error("[4] See file Dichromate.help.")
        end_if;
        break;
      otherwise error("[5] Too many arguments !! See file Dichromate.help.")
    end_case;
                
    forcep:=aux[4];/* Utiliser Dichromate::Pi et non la serie */
    r:=aux[1];
    RefEdges:=aux[2];
    s:=aux[3];
    if type(DiChromateq)<> DOM_IDENT then
      oldDiChromateq:=DiChromateq;
      unassign(DiChromateq)
    else
      oldDiChromateq:=FALSE
    end_if;
    /* Initialisation */
    Dichromate::MakeAll(r,RefEdges,TRUE);
    userinfo(1,"Fin de l'initialisation ... Debut du boulot !");

    if (s<> hold(All)) and (forcep or s<r^2) then
      Dichromate::MakeMoeb(r);
      res:=Dichromate::Pi(s,RefEdges);
    else
      userinfo(1,"Computing the whole series...");
      WholeSeries:=Dichromate::IGetSeries(RefEdges);
      if s=hold(All) then
        res:=WholeSeries
      else
        res:=op(taylor(Dom::Expression(WholeSeries),
                            hold(X),s+1),[4,s])
      end_if;
    end_if;

    if type(r)=DOM_INT then
      k:=1
    else
      k:=nops(Dichromate::ConnectedCompo(G))
    end_if;

    if s=hold(All) then
      res:=normal(Dichromate::Simpler(
        (DiChromatex-1)^(-k)*(DiChromatey-1)^(-r)*
      subs(res,[DiChromateq=(DiChromatex-1)*(DiChromatey-1),
                DiChromatev=DiChromatey-1,
                hold(X)=hold(X)/(DiChromatey-1)^r])))
    else
      res:=normal(Dichromate::Simpler(
        (DiChromatex-1)^(-k)*(DiChromatey-1)^(-r*s)*
        subs(res,[DiChromateq=(DiChromatex-1)*(DiChromatey-1),
                  DiChromatev=DiChromatey-1])))
    end_if;
    if oldDiChromateq then DiChromateq:=oldDiChromateq
    end_if;
    return(res)
  end_proc:

/*----------------------------------------------------------------*/

//Dichromate::RectTutte(2,2);
//Dichromate::MakeAll(3,[[1,2],[2,3]],FALSE):
//Dichromate::InitialVect([[1,2],[2,3]]);
//Dichromate::RectDiChromatic(2,hold(All));
//Dichromate::RectDiChromatic([{1,2,3},[[1,2],[2,3],[3,1]]],1);

/*   That's all, folks ! ------------- 982 lines ----------------*/
