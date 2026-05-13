/*
  javac -cp /usr/lib/jvm/java-6-sun-1.6.0.26/jre/lib/plugin.jar:. PlateauSolitaire.java 
*/

public class PlateauSolitaire
{
    public static final int OUTOFTABLE = 9;
    public static final int NBBASEHASH = 8; // Beware, it is often hardcoded !
    String name = new String();
    int hauteur, largeur, nbcases, nbcoups;
    int table[];
    int baseHash[]; // Doit contenir 8 entiers
    Coup coups[];
    int listePotElem[][]; // liste des potelem
    int listeCarac[][]; // liste des carac
    // Attention !! Les potentiels n'ont pas un 9 aux cases manquantes.
    int pcodeToCode[]; // pcodeToCode[pcode] is the code via xytocode of the point
    int pcodeToPoint[][]; // pcodeToPoint[acode] = [Px, Py]
    int codeToPoint[]; // codeToPoint[code] = [Px, Py]
    /**  Le code des points est tel que codeQ est entre codeP et codeR **/
    int codeToPcode[]; // codeToPcode[code] = pcode if code is valid, NaN otherwise
    double listeContraintesLin[][]; // listeContraintesLinEx[pcodepoint][codecoup] = coup(point)

    public PlateauSolitaire(int myhauteur, int mylargeur, int[] mytable, String myname)
    {   this.hauteur = myhauteur;
        this.largeur = mylargeur;
        this.table = mytable; /* on ne le recopie pas !! */
        this.name = myname;  
        /*** init is done via **precalculs** below ***/
    }

    public int xytocode(int x, int y)
    /* 0 <= x < myhauteur, 0 <= y < mylargeur */
    {   return(y + x * this.largeur); 
    }

    public void createPcodeToCode(){
        int nb = 0;
        this.pcodeToCode = new int[this.nbcases];
        for( int x = 0; x < this.hauteur; x++){
            for( int y = 0; y < this.largeur; y++){
                if( this.table[ this.xytocode( x, y)] != OUTOFTABLE){ 
                    this.pcodeToCode[nb] = this.xytocode( x, y);
                    nb++;}}}
    }

    public void createCodeToPcode(){
        int nb = 0;
        this.codeToPcode = new int[this.largeur*this.hauteur];
        for( int pc = 0; pc < this.nbcases; pc++){
            this.codeToPcode[this.pcodeToCode[pc]] = pc;} 
    }

    public void createPcodeToPoint(){
        this.pcodeToPoint = new int[this.nbcases][2];
        for( int x = 0; x < this.hauteur; x++){
            for( int y = 0; y < this.largeur; y++){
                if( this.table[ this.xytocode( x, y)] != OUTOFTABLE){ 
                    this.pcodeToPoint[this.codeToPcode[xytocode( x, y)]][0] = x;
                    this.pcodeToPoint[this.codeToPcode[xytocode( x, y)]][1] = y;}}}
    }

    class WrongFormat extends Exception{
        WrongFormat (String mes){
            super(mes);
        }
    }

    public void createListeContraintesLin(){
         // listeContraintesLinEx[pcodepoint][codecoup] = coup(point)
        this.listeContraintesLin = new double[this.nbcases][this.nbcoups];
        for(int c = 0; c < this.nbcoups-1; c++){
            int pcP = this.codeToPcode[this.xytocode(this.coups[c].Px, this.coups[c].Py)];
            int pcQ = this.codeToPcode[this.xytocode(this.coups[c].Qx, this.coups[c].Qy)];
            int pcR = this.codeToPcode[this.xytocode(this.coups[c].Rx, this.coups[c].Ry)];
            this.listeContraintesLin[pcP][c] = 1;
            this.listeContraintesLin[pcQ][c] = 1;
            this.listeContraintesLin[pcR][c] = -1;}
   }

    public int calculeNbcases(){
        int x, y, nb = 0;
        
        for( x = 0; x < this.hauteur; x++){
            for( y = 0; y < this.largeur; y++){
                if( this.table[ this.xytocode( x, y)] != OUTOFTABLE){ nb++;}}}
        return nb;
    }

    public Coup[] calculeCoups(){
        int x, y, nb = 0;
    
        /* D'abord on calcule la taille !! */
        /* Coups horizontaux : */
        for( x = 0; x < this.hauteur; x++){
            for( y = 0; y < this.largeur-2; y++){
                if((this.table[this.xytocode(x,y)] != OUTOFTABLE)
                   &(this.table[this.xytocode(x,y+1)] != OUTOFTABLE)
                   &(this.table[this.xytocode(x,y+2)] != OUTOFTABLE)){ nb += 2;}}}

        /* Coups verticaux : */
        for( x = 0; x < this.hauteur-2; x++){
            for( y = 0; y < this.largeur; y++){
                if((this.table[this.xytocode(x,y)] != OUTOFTABLE)
                   &(this.table[this.xytocode(x+1,y)] != OUTOFTABLE)
                   &(this.table[this.xytocode(x+2,y)] != OUTOFTABLE)){ nb += 2;}}}

        /* Maintenant on affecte */
        Coup[] theCoups = new Coup[nb];
        nb = 0;
        /* Coups horizontaux : */
        for( x = 0; x < this.hauteur; x++){
            for( y = 0; y < this.largeur-2; y++){
                if((this.table[this.xytocode(x,y)] != OUTOFTABLE)
                   &(this.table[this.xytocode(x,y+1)] != OUTOFTABLE)
                   &(this.table[this.xytocode(x,y+2)] != OUTOFTABLE)){
                    theCoups[nb++] = new Coup(x, y, x, y+1, x, y+2);
                    theCoups[nb++] = new Coup(x, y+2, x, y+1, x, y);
                }}}

        /* Coups verticaux : */
        for( x = 0; x < this.hauteur-2; x++){
            for( y = 0; y < this.largeur; y++){
                if((this.table[this.xytocode(x,y)] != OUTOFTABLE)
                   &(this.table[this.xytocode(x+1,y)] != OUTOFTABLE)
                   &(this.table[this.xytocode(x+2,y)] != OUTOFTABLE)){
                    theCoups[nb++] = new Coup(x, y, x+1, y, x+2, y);
                    theCoups[nb++] = new Coup(x+2, y, x+1, y, x, y);
                }}}

        return(theCoups);
    }

    public void precalculs ()
    {
        this.nbcases = calculeNbcases();
        this.coups = calculeCoups();
        this.nbcoups = coups.length;
        createPcodeToCode();
        createCodeToPcode();
        createPcodeToPoint();
        createListeContraintesLin();
    }
}