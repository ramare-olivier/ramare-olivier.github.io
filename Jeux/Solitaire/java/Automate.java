import java.util.*; // pour Stack
import lpsolve.*;

public class Automate implements Runnable
{
    //static TestLineaire TL;
    static Communicator cU;
    static CommQueue q;
    Etude myJeu;
    PlateauSolitaire myPlateau;
    
    public Automate (Communicator commUnit, CommQueue q, Etude myJeu, PlateauSolitaire myPlateau){
        this.cU = commUnit;
        this.q = q;
        this.myJeu = myJeu;
        this.myPlateau = myPlateau;
        //this.TL = new TestLineaire(cU);
        //this.TL = new TestLineaire();
        new Thread(this, "Automate").start();
    }

    public static int HashCode(int pos[], PlateauSolitaire myPlateau){
        return pos[myPlateau.baseHash[0]] + pos[myPlateau.baseHash[1]]*2
        + pos[myPlateau.baseHash[2]]*4 + pos[myPlateau.baseHash[3]]*8
        + pos[myPlateau.baseHash[4]]*16 + pos[myPlateau.baseHash[5]]*32
        + pos[myPlateau.baseHash[6]]*64 + pos[myPlateau.baseHash[7]]*128;
    }

    public static void JoueCoup(int pos[], Coup coup, PlateauSolitaire myPlateau){
        pos[myPlateau.xytocode(coup.Px, coup.Py)] = 0;
        pos[myPlateau.xytocode(coup.Qx, coup.Qy)] = 0;
        pos[myPlateau.xytocode(coup.Rx, coup.Ry)] = 1;
    }

    public static Coup CoupPrime(Coup acoup){
        return new Coup(acoup.Rx, acoup.Ry, acoup.Qx, acoup.Qy, acoup.Px, acoup.Py);
    }

    public static int[] JoueChemin(int pos[], Stack chemin, PlateauSolitaire myPlateau){
        int posIni[] = new int[myPlateau.hauteur*myPlateau.largeur];

        for(int c = 0; c < myPlateau.hauteur*myPlateau.largeur; c++){
            posIni[c] = pos[c];}

        for(int n = 0; n < chemin.size(); n++){
            JoueCoup(posIni, (Coup) chemin.get(n), myPlateau);};
        return posIni;
    }

    public static boolean EqPos(int pos1[], int pos2[]){
        for(int i = 0; i < pos1.length; i++){
            if(pos1[i] != pos2[i]){return false;}}
        return true;
    }

    public static int[] InvertPos(int pos[], PlateauSolitaire myPlateau){
        int newPos[] = new int[pos.length];
        for(int c = 0; c < myPlateau.hauteur*myPlateau.largeur; c++){
            if(myPlateau.table[c] == 9){
                newPos[c] = pos[c];}
            else { newPos[c] = 1-pos[c];}}
        return newPos;
    }

    public static Stack CoupsPossibles (int pos[], Stack chemin, PlateauSolitaire myPlateau) {
    /* pos initiale = pos "-" chemin */
        int posIni[];
        Coup acoup; 
        Stack listeCoups = new Stack();

        if(chemin.size() == 0){
            posIni = pos; // chemin est vide
        } else {
            posIni = JoueChemin(pos, chemin, myPlateau);
        }
        //cU.Alert("pos = "+ cU.PosToString(pos));
        //cU.Alert("posIni = "+ cU.PosToString(posIni));
        for(int c = 0; c< myPlateau.coups.length; c +=2){
            acoup = myPlateau.coups[c];
            //cU.Alert("Coup numero "+c);
            if(posIni[myPlateau.xytocode(acoup.Qx, acoup.Qy)]==1){
                if((posIni[myPlateau.xytocode(acoup.Px, acoup.Py)]==1)
                   &(posIni[myPlateau.xytocode(acoup.Rx, acoup.Ry)]==0)){
                    //cU.Alert("Coup possible ");
                    listeCoups.push(acoup);
                } else if ((posIni[myPlateau.xytocode(acoup.Px, acoup.Py)]==0)
                           &(posIni[myPlateau.xytocode(acoup.Rx, acoup.Ry)]==1)){
                    //cU.Alert("Coup possible (type 2) ");
                    listeCoups.push(myPlateau.coups[c+1]);
                }}}
        return listeCoups;
    }

    public static Stack CheminsPossibles(int posIni[], int longueur, PlateauSolitaire myPlateau){
        Stack achemin = new Stack();
        Stack chemins = new Stack();
        Stack cp;

        chemins.push(achemin);
        for(int i = 1; i<= longueur; i++){
            Stack newchemins = new Stack();
            //cU.Alert(""+chemins.size()+ " chemins ...");
            for(int n = 0; n < chemins.size(); n++){
                cp = CoupsPossibles(posIni, (Stack) chemins.get(n), myPlateau);
                //cU.Alert(""+cp.size()+ " coups possibles ...");
               
                for(int c = 0; c < cp.size(); c++){
                    if(((Stack) chemins.get(n)).size() == 0){
                        Stack newone = new Stack ();
                        newone.push((Coup) cp.get(c));
                        newchemins.push(newone);
                    } else {
                        Stack newone = new Stack ();
                        /* Cloning chemins.get(n) : */
                        for(int d = 0; d < ((Stack) chemins.get(n)).size(); d++){
                            newone.push((Coup) ((Stack) chemins.get(n)).get(d));}
                        newone.push((Coup) cp.get(c));
                        newchemins.push(newone);
                    }}}
            chemins = newchemins;
            if(chemins.size() == 0){return achemin;}}
        return chemins;
    }

    public static boolean AlreadyAtteinte(Derivation myder, Stack derivations,
                                          PlateauSolitaire myPlateau){

        if(derivations.size() == 0){return false;};
 
        for(int nb = 0; nb < derivations.size(); nb++){
            if(myder.hashCode == ((Derivation) derivations.get(nb)).hashCode){
                if(EqPos(myder.positionAtteinte, 
                         ((Derivation) derivations.get(nb)).positionAtteinte) == true){
                    return true;
                }}}
        return false;
    }

    public static Stack DerivationsPossibles(int posIni[], int longueur, PlateauSolitaire myPlateau){
        int nbkilledbyhash = 0;
        Stack derivations = new Stack();
        Stack chemins = CheminsPossibles(posIni, longueur, myPlateau);

        //cU.Alert(""+chemins.size()+" chemins possibles !");
        for(int c = 0; c < chemins.size(); c++){
            int newPos[] = JoueChemin(posIni, (Stack) chemins.get(c), myPlateau);
            Derivation myder = new Derivation((Stack) chemins.get(c), newPos, 
                                              HashCode(newPos, myPlateau));
            
            if(AlreadyAtteinte(myder, derivations, myPlateau) == false){
                derivations.push(myder);
                } else {nbkilledbyhash++;};
        }
        //alert(nbkilledbyhash + "derivations killed by hashtable");
        return derivations;
    }

    public static Stack DerivationsPossiblesPlus(Stack derivationsIni, int longueur, 
                                          PlateauSolitaire myPlateau){
        Stack derivations = new Stack();
        int nbkilledbyhash = 0;
        
        for(int qui = 0; qui < derivationsIni.size(); qui++){   
            int posIni[] = ((Derivation) derivationsIni.get(qui)).positionAtteinte;
            Stack chemins = CheminsPossibles(posIni, longueur, myPlateau);
            Stack cheminInitial = (Stack) ((Derivation) derivationsIni.get(qui)).chemin;
            for(int c = 0; c < chemins.size(); c++){
                int newPos[] = JoueChemin(posIni, (Stack) chemins.get(c), myPlateau);
                Stack debut = new Stack();
                Stack cp = (Stack) chemins.get(c);
                for(int a = 0; a< cheminInitial.size(); a++){
                    debut.push((Coup) cheminInitial.get(a));}
                //cU.Alert("old length of chemin = "+debut.size());
                for(int a = 0; a< cp.size(); a++){
                    debut.push((Coup) cp.get(a));}
                Derivation myder = new Derivation(debut, newPos, HashCode(newPos, myPlateau));
                
                //cU.Alert("New length of chemin = "+debut.size());
                if(AlreadyAtteinte(myder, derivations, myPlateau) == false){
                    derivations.push(myder);
                } else { nbkilledbyhash++;};
            }}
        //alert("[plus] "+nbkilledbyhash + "derivations killed by hashtable");
        return derivations;
    }

    /*----------------------------------------------------------------*/
    /*----------------------------------------------------------------*/
    /*----------------------------------------------------------------*/
    /*----------------------------------------------------------------*/



    /*----------------------------------------------------------------*/
    /*----------------------------------------------------------------*/
    /*----------------------------------------------------------------*/
    /*----------------------------------------------------------------*/

    public static Stack EssaieDeDeriverInner(Etude myJeu, PlateauSolitaire myPlateau, 
                                     int nbdone, int nbsteps, int noBranche){
        int nb = Math.min(myJeu.longueurPasDescente, nbsteps - nbdone);
        if((nbsteps > nbdone)&&(myJeu.derivations.size() > 0)){
            myJeu.derivations = 
                DerivationsPossiblesPlus(myJeu.derivations, nb, myPlateau);
            nbdone += nb;
            q.receive("BRANCH <br/>" + cU.Mesg(15) + myJeu.derivations.size()
                      + cU.Mesg(16) + nbdone + cU.Mesg(17));
            UtilitairesJava.NettoyageParPotElem(myJeu, myPlateau);
            q.receive("BRANCH <br/><img src='../../trucs/arrowhead-right.jpg'>&nbsp; " +
                      cU.Mesg(18) + myJeu.derivations.size() + cU.Mesg(19));
            
            //nbsteps += -nb; 
            return EssaieDeDeriverInner(myJeu, myPlateau, nbdone, nbsteps, noBranche);
        } else {
            return EssaieDeDeriverConclude(myJeu, myPlateau, nbdone, nbsteps, noBranche);
        }
    } 

    public static Stack EssaieDeDeriverConclude(Etude myJeu, PlateauSolitaire myPlateau, 
                                                int nbdone, int nbsteps, int noBranche){
        int done = 0;
    
        if(myJeu.derivations.size() == 0){
            done = 0;
        } else {
            for(int d = 0; d < myJeu.derivations.size(); d++){
                if(EqPos(((Derivation) myJeu.derivations.get(d)).positionAtteinte,
                         myJeu.positionFinale) == false){
                    myJeu.derivations.remove(d);
                    d--;}}
            if(myJeu.derivations.size() == 0){done = 0;} else {done = 1;};
        };
       
        if(done == 0){
            //cU.Alert("</div> type 1");
            q.receive("FOLD");//cU.Alert("FOLDING?");
            q.receive("BRANCH &nbsp;&nbsp;<span style='color:#A00000'>"
                      + cU.Mesg(43) + "</span>");
            return new Stack();
        } else  {
            return ((Derivation) myJeu.derivations.get(0)).chemin;
       }
    }   
    
    public static Stack EssaieDeDeriver(Etude myJeu, PlateauSolitaire myPlateau, int noBranche){
        /* noBranche est entre 1 et max... et non entre 0 et max...-1 */
        //cU.Alert("Starting EssaieDeDeriver ...");
        int nbsteps = UtilitairesJava.NbPions(myJeu.positionInitiale, myPlateau)
            - UtilitairesJava.NbPions(myJeu.positionFinale, myPlateau);
        //cU.Alert(String.valueOf(nbsteps));
        int nbdone = Math.min(myJeu.longueurPasDescente, nbsteps);
        myJeu.derivations = DerivationsPossibles(myJeu.positionInitiale, 
                                                 nbdone, myPlateau);
        q.receive("<div id='tobefolded-" + noBranche + "'></div>");
        q.receive("OPENBRANCH "+noBranche);
        q.receive("BRANCH " + cU.Mesg(15) 
                  + myJeu.derivations.size()
                  + cU.Mesg(16)+  myJeu.longueurPasDescente + cU.Mesg(17));
        //cU.Alert(String.valueOf(myJeu.derivations.size()));
        UtilitairesJava.NettoyageParPotElem(myJeu, myPlateau); // !! Todo!!
        q.receive("BRANCH <br/><img src='../../trucs/arrowhead-right.jpg'>&nbsp; "
                  + cU.Mesg(18) + myJeu.derivations.size() + cU.Mesg(19));

        //nbsteps += -nbdone;
        return EssaieDeDeriverInner(myJeu, myPlateau, nbdone, nbsteps, noBranche);
    }

    public static void EssaieDeDeriverWithBranches(Etude myJeu, PlateauSolitaire myPlateau){
        int nbsteps = UtilitairesJava.NbPions(myJeu.positionInitiale, myPlateau)
            - UtilitairesJava.NbPions(myJeu.positionFinale, myPlateau);
        int invertedPos[] = InvertPos(myJeu.positionFinale, myPlateau);
        int howManyToGoUp = Math.min(Math.max(nbsteps -1, 0), myJeu.remonterDeTant);

        Stack chemins = CheminsPossibles(invertedPos, howManyToGoUp, myPlateau);
 
        q.receive("<br/><img src='../../trucs/arrow-shadowed-outline-right.jpg'>&nbsp; "
                       + cU.Mesg(38) + chemins.size() + cU.Mesg(39)
                       + howManyToGoUp+ cU.Mesg(40)+"<br/>");
        //q.receive("<br/>" + cU.PosToString(myJeu.positionFinale));
       //cU.Alert(cU.Mesg(38) + chemins.size() + cU.Mesg(39));
        int oldPosFin[] = myJeu.positionFinale;
        Stack resultat = new Stack();
        int aFinal = 0;
        for(int a = 0; a < chemins.size(); a++){
            int posFin[] = InvertPos(JoueChemin(invertedPos, 
                                                (Stack) chemins.get(a), myPlateau),
                                     myPlateau);
            myJeu.positionFinale = posFin;
            cU.Comm("derivedviajava", "&nbsp;&nbsp;Working "
                    + (a+1) + "/" + (chemins.size()) + " ...");
            q.receive("<img src='../../trucs/arrowhead-down.jpg'>&nbsp; "
                      + "<span style='background-color:#E9CFEC;font-style:oblique;'>"
                      + cU.Mesg(41) + (a+1) + cU.Mesg(42) + "&nbsp;</span>");
            if(myJeu.showBranches){
                q.receive("<br/>" + cU.PosToStringHTML(myJeu.positionFinale, myPlateau));
            }
            if(UtilitairesJava.isOkParPotElem(myJeu.positionInitiale, posFin, myPlateau) == false){
                q.receive(" No way by PotElem!<br/>");
                continue;
            }
            /*----------------PFOU!!!!!!!!!!!!!-----------------*/
            try{
                cU.Alert("Going to Tests!!!");
                LpSolve mylp = LpSolve.makeLp(0, 0);
                cU.Alert("In FaisableEnRationnelsPositifs ... ");
                SolListener listener = new SolListener(cU);
                mylp.putLogfunc(listener, new Integer(123));
            } catch (LpSolveException lpse){
                cU.Alert("Exception in FaisableEnRationnelsPositifs!!");
                System.out.println("Ouinnnn ");
                lpse.printStackTrace();
            }
            /*
              if(TL.FaisableEnRationnelsPositifs() == false){
                q.receive(" No way real potentiels!<br/>");
                continue;
            }
            q.receive(" Test en rationels positifs donne : peut etre ok ...<br/> ");
            */
            /*----------------PFOU!!!!!!!!!!!!!-----------------*/
            
            resultat = EssaieDeDeriver(myJeu, myPlateau, a+1);
            if(resultat.size() != 0){ aFinal = a; break;}
        }
        if(resultat.size() != 0){
            /* Add the path to the partial final position to the real final position :*/
            Stack lastChemin =  (Stack) chemins.get(aFinal);
            for(int a = lastChemin.size()-1; a >-1; a--){
                resultat.push((Coup) lastChemin.get(a));
            }
            /* Restore myJeu: (though it is of no use up to now:) !!)  */
            myJeu.positionFinale = oldPosFin;

            q.receive("<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style='color:#FF00FF'>"
                               + cU.Mesg(21) + "</span>");
            cU.CommPlus("play_derivation",
                     "<input type='button' style='width:250px;background-color:#FFFFCC'"
                     + " onClick='play_derivation()' value='PlayIt!'>");
            cU.CommPlus("restore_ini",
                     "<input type='button' style='width:200px;background-color:#FFFFCC'"
                     + " onClick='restore_ini()' value='Restore position initiale'>");
            cU.CommPlus("show_path",
                     "<input type='button' style='width:200px;background-color:#FFFFCC'"
                     + " onClick='alert(ecrire_chemin(Etude.derivations[0].chemin))' value='"
                     + cU.Mesg(22) + "'>");
            /* Pour que javascript puisse afficher cette derivation, il faut affecter
               Etude.derivations[0], qui est une liste de coup. */
            cU.TransfertLastDerivation((Derivation) myJeu.derivations.get(0));
            cU.Comm("derivedviajava", "&nbsp;&nbsp;Done!");
        } else {
            q.receive("<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
                      + "<span style='color:#A00000;background-color:#E9CFEC;font-style:oblique;'>"
                      +cU.Mesg(20) + "</span>");
            cU.Comm("derivedviajava", "&nbsp;&nbsp;No way!");
        }
    }

    public void run(){
        EssaieDeDeriverWithBranches(myJeu, myPlateau);
    }

}

