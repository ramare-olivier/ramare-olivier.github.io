//import javax.swing.*;


/* To be compiled with 
   javac -cp /usr/lib/jvm/java-6-sun-1.6.0.26/jre/lib/plugin.jar:. UtilitairesJava.java 
*/

public class UtilitairesJava
{
    public static int NbPions(int pos[], PlateauSolitaire myPlateau){
        int nb = 0;
        for(int c = 0; c< myPlateau.hauteur*myPlateau.largeur; c++){
            if(myPlateau.table[c] != 9){
                nb += pos[c];}};
        return nb;
    }
        
    /*------------- Cleaners ----------------------------------*/

    public static int ScorePot(int posIni[], int posFin[], int pot[], PlateauSolitaire myPlateau){
        int score = 0;
        for(int c = 0; c < myPlateau.hauteur*myPlateau.largeur; c++){
            if(myPlateau.table[c]!=9){
                score += (posIni[c]-posFin[c])*pot[c];}}
        return score;
    }
    
    public static boolean isOkParPotElem(int posIni[],int posFin[], PlateauSolitaire myPlateau){
        for(int c = 0; c< myPlateau.listePotElem.length; c++){
            if(ScorePot(posIni, posFin,
                        myPlateau.listePotElem[c], myPlateau) < 0) {
                return false;}
        }
        return true;
    }

    public static void NettoyageParPotElem(Etude myJeu, PlateauSolitaire myPlateau){
        for(int qui = 0; qui < myJeu.derivations.size(); qui++){
            int posAtt[] = ((Derivation) myJeu.derivations.get(qui)).positionAtteinte;
            for(int c = 0; c< myPlateau.listePotElem.length; c++){
                if(ScorePot(posAtt, myJeu.positionFinale, 
                            myPlateau.listePotElem[c], myPlateau) < 0) {
                    myJeu.derivations.remove(qui);
                    c = myPlateau.listePotElem.length;
                    qui--;}}}
    }

}