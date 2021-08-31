
public class Auto implements Runnable
{
    public Auto(){
        new Thread(this, "Yo").start();
    }
    
    public void run (){
    }
                  

    public static void main(String[] args){
        Auto aut=new Auto();
       int[] posIni = new int[16];
        int[] posFin = new int[16];
        int[] myTable = new int[16];
        for(int c = 0; c < 16; c++){
            posIni[c] = 1; posFin[c] = 1; myTable[c] = 1; 
        }
        PlateauSolitaire myPlateau = new PlateauSolitaire(4, 4, myTable, "Essai");
        System.out.println(TestLineaire.FaisableEnRationnelsPositifs(posIni, posFin, myPlateau));
    }
}