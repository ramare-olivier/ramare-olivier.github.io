import java.util.*;  // pour Stack

public class Etude 
{
    int positionInitiale[];
    int positionFinale[];
    Stack derivations;
    int longueurPasDescente;
    int remonterDeTant;
    boolean showBranches ;
    
    public Etude(int posIni[], int posFin[]){
        this.positionInitiale = posIni;
        this.positionFinale = posFin;
        this.derivations = new Stack();
    }
}