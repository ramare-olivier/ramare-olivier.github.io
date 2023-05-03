import netscape.javascript.*;
import java.util.*; // pour Stack

public class Communicator{

    static JSObject mainWindow;
    static JSObject mainDocument;
    static JSObject derivedInnerHTML;

    public Communicator (JSObject win){
        this.mainWindow = win;
        this.mainDocument = (JSObject) mainWindow.getMember("document");
        this.derivedInnerHTML = (JSObject) win.eval("document.getElementById('derived')");
    }

    /*------------- Comm ----------------------------------*/

    public static void Comm(String qui, String quoi){
        mainWindow.eval("document.getElementById(\""+ qui + "\").innerHTML = \"" + quoi + "\";");
    }
    
    public static void CommDerived(String quoi){
        derivedInnerHTML.setMember("innerHTML", quoi);
    }
    
    public static void CommPlus(String qui, String quoi){
        mainWindow.eval("document.getElementById(\""+ qui + "\").innerHTML += \""+ quoi + "\";");
    }
    
    public static void CommPlusDerived(String quoi){
        //mainWindow.eval("document.getElementById(\""+ qui + "\").innerHTML += \""+ quoi + "\";");
        derivedInnerHTML.setMember("innerHTML", derivedInnerHTML.getMember("innerHTML") + quoi);
    }
    
    public static String Mesg(int no){
        return mainWindow.eval("message["+no+"][language];").toString();
    }

    public static void Alert(String what){
        mainWindow.eval("alert(\'" + what + "\');");
    }

    public static String PosToString(int pos[]){
        String mesg = "";
        for(int c = 0; c<pos.length; c++){
            mesg += pos[c]+" ";};
        return mesg;
    }
    
    public static String PosToStringHTML(int pos[], PlateauSolitaire myPlateau){
        String mesg = "<table>";
        for(int x = 0; x < myPlateau.hauteur; x++){
            mesg += "<tr>";
            for(int y = 0; y < myPlateau.hauteur; y++){
                if(myPlateau.table[myPlateau.xytocode(x, y)] == 9){
                    mesg += "<td></td>";
                } else {
                    mesg += "<td style='background-color:#00FFFF;'>" + pos[myPlateau.xytocode(x, y)] + "</td>";
                }}
            mesg += "</tr>";}
        mesg += "</table>";
        return mesg;
    }
    
 

    /*------------- Transferts -------------------------------*/

    public static void TransfertLastDerivation(Derivation winner){
        /* Pour que javascript puisse afficher cette derivation, il faut affecter
           Etude.derivations[0], qui est une liste de coup. */
        String commande = "var cc, chemin = new Array();";
        Stack chem = winner.chemin;
        
        for(int i = 0; i<chem.size(); i++){
            Coup cp = (Coup) chem.get(i);
            commande += "cc = new coup("+cp.Px+","+cp.Py+","
                +cp.Qx+","+cp.Qy+","+cp.Rx+","+cp.Ry+");chemin.push(cc);";}
        commande += "var myder = new derivation(chemin, Etude.position_finale,"
            +"hashcode(Etude.position_finale, PlateauEtude));";
        commande += "Etude.derivations=new Array();Etude.derivations.push(myder);";
        
        mainWindow.eval(commande);
    }

}