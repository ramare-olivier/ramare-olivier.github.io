import java.util.*; // pour Stack

public class Derivation
{
    public Derivation (Stack mypath, int pos[], int myhash){
        this.chemin = mypath; // une liste de coups
        this.positionAtteinte = pos; 
        this.hashCode = myhash; // hashcode de la position atteinte
    }

    Stack chemin;
    int positionAtteinte[];
    int hashCode;
}