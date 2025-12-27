import netscape.javascript.*;
import java.applet.*;
import java.awt.*;

/* To be compiled with 
   javac -cp /usr/lib/jvm/java-6-sun-1.6.0.26/jre/lib/plugin.jar:. UtilitairesJava.java 
*/

public class PegSolApplet extends Applet 
{
    static PlateauSolitaire plateauEtude;
    Etude etude;
    static TalkativeAutomate TA;
    static JSObject mainWindow;

    public void init() {
        /* methode lancee au chargement de l'applet */
        //mainWindow.eval("alert('There I am! 32');");
        mainWindow = JSObject.getWindow(this);
    }

    // Le plateauEtude : 
    
    public void initPlateauEtude() {
        int alargeur, ahauteur;
        String aname;
        
        /* copy plateauEtude from its Javascript counterpart. */
        ahauteur =
            Integer.parseInt(mainWindow.eval("PlateauEtude.hauteur").toString());
        alargeur =
            Integer.parseInt(mainWindow.eval("PlateauEtude.largeur").toString());
        aname = mainWindow.eval("PlateauEtude.name").toString();

        int myTable[] = new int[ahauteur*alargeur];
        for(int c = 0; c< ahauteur*alargeur; c++){
            myTable[c] = 
                Integer.parseInt(mainWindow.eval("PlateauEtude.getTable("+c+")").toString());
        }
        plateauEtude = new PlateauSolitaire(ahauteur, alargeur, myTable, aname);
        
        int myBasehash[] = new int[8];
        for(int c = 0; c < 8; c++){
            myBasehash[c] = 
                Integer.parseInt(mainWindow.eval("PlateauEtude.getbase_hash("+c+")").toString());
        }
        plateauEtude.baseHash = myBasehash;

        int taille = 
            Integer.parseInt(mainWindow.eval("PlateauEtude.getlengthliste_carac()").toString());
        int mylisteCarac[][] = new int[taille][ahauteur*alargeur];
        for(int a = 0; a < taille; a++){
            for(int c = 0; c< ahauteur*alargeur; c++){
                mylisteCarac[a][c] = 
                    Integer.parseInt(mainWindow.eval("PlateauEtude.getliste_carac("+a+","+c+")").toString());
            }}
        plateauEtude.listeCarac = mylisteCarac;

        taille = 
            Integer.parseInt(mainWindow.eval("PlateauEtude.getlengthliste_pot_elem()").toString());
        int mylistePotElem[][] = new int[taille][ahauteur*alargeur];
        for(int a = 0; a < taille; a++){
            for(int c = 0; c< ahauteur*alargeur; c++){
                mylistePotElem[a][c] = 
                    Integer.parseInt(mainWindow.eval("PlateauEtude.getliste_pot_elem("+a+","+c+")").toString());
            }}
        plateauEtude.listePotElem = mylistePotElem;

        // On calcule les coups et le nombre de cases:
        plateauEtude.precalculs();
    }

    // l'etude :
    public void initEtude() {
        int taille = this.plateauEtude.hauteur*this.plateauEtude.largeur;
        int posIni[] = new int[taille];
        
        /* copy etude from its Javascript counterpart. */
        for(int c = 0; c< taille; c++){
            posIni[c] = 
                Integer.parseInt(mainWindow.eval("Etude.getPosIni("+c+")").toString());
        }
        
        int posFin[] = new int[taille];
        /* copy etude from its Javascript counterpart. */
        for(int c = 0; c< taille; c++){
            posFin[c] = 
                Integer.parseInt(mainWindow.eval("Etude.getPosFin("+c+")").toString());
        }
        
        etude = new Etude(posIni, posFin);

        etude.longueurPasDescente = 
            Integer.parseInt(mainWindow.eval("longueur_pas_descente").toString());
        etude.remonterDeTant =
            Integer.parseInt(mainWindow.eval("remonter_de_tant").toString());
        etude.showBranches =
            Boolean.parseBoolean(mainWindow.eval("show_branches").toString());
    }

    /*-------------------------------------------------*/

    public void JavaGoesToWork(){
         initEtude();
         //Alert((String) "Out of initEtude");
         TA = new TalkativeAutomate(this.etude, this.plateauEtude, this.mainWindow);
    }
    
}