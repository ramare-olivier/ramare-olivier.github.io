/* SolitaireEtude.js : un programme pour etudier le solitaire
 * Copyright (C) 2012 (Olivier Ramar\'e)
 * Ce programme est libre, vous pouvez le redistribuer et/ou le modifier selon 
 * les termes de la Licence Publique Generale GNU publiee par la Free Software Foundation
 *  (version 2 ou bien toute autre version ulterieure choisie par vous). 
 * Ce programme est distribue car potentiellement utile, mais SANS AUCUNE GARANTIE, ni explicite
 * ni implicite, y compris les garanties de commercialisation ou d'adaptation dans un but specifique.
 * Reportez-vous a la Licence Publique Generale GNU pour plus de details.
 * Vous devez avoir recu une copie de la Licence Publique Generale GNU en meme temps que ce programme ; 
 * si ce n'est pas le cas, ecrivez a la 
 * Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, Etats-Unis. 
 * 
 * **  
 * ** 
 * -----------------------------------------------------------------------*/

var crossedoutcolor = "#0404B4";
var largeurmaximale = 10;
var hauteurmaximale = 10;
var verrouplateau = 0; /* Si 0, on ne peut plus modifier le plateau */
var PlateauEtude;
var Etude;

<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->
<!-- -------------------------------------- -->
var PlateauMaximal = new PlateauSolitaire(hauteurmaximale, largeurmaximale, 
                                          Populated_array(hauteurmaximale*largeurmaximale, 0),
                                          "Plateau_Graphique");
<!-- -------------------------------------- -->
includeJS("Plateaux/Plateaux.js");
<!-- -------------------------------------- -->

function uneEtude(pos_ini, pos_fin) {
    this.position_initiale = pos_ini;
    this.position_finale = pos_fin;
    this.selectedstartx = -1; // -1: aucune case de selectionnee 
    this.selectedstarty = -1; // -1: aucune case de selectionnee 
    this.problematique_message = "";
    this.derivations = [];
}

/* The getters are for Java: */
uneEtude.prototype.getPosIni = function (c){
    return(this.position_initiale[c]);}

uneEtude.prototype.getPosFin = function (c){
    return(this.position_finale[c]);}


<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->
function Plateau_Graphique_mycoloring(x, y, mycolor, myPlateau){
    document.getElementById(myPlateau.name+"_button_"+lettres[x]+lettres[y]).style.background=mycolor;
    return;
}

function ecrire_chemin(achemin) {
    var messg = "";
    for(var c=0; c< achemin.length; c++){
        messg += ecrire_coup(achemin[c])+" ";}
    return messg;
}

function Plateau_Graphique_select(x, y, myPlateau) {
    if(verrouplateau == 1){ alert(message[0][language]); return;};
    //alert(myPlateau.Table[myPlateau.xytocode(x, y)]);
    if (myPlateau.Table[myPlateau.xytocode(x, y)] == 0) {
        myPlateau.Table[myPlateau.xytocode(x, y)] = 9;
        Plateau_Graphique_mycoloring(x, y, crossedoutcolor, myPlateau);
    } else {
        myPlateau.Table[myPlateau.xytocode(x, y)] = 0;
        hauteurmaximale = mymax(hauteurmaximale, x);
        largeurmaximale = mymax(largeurmaximale, y);
        Plateau_Graphique_mycoloring(x, y, initialcolor, myPlateau);
    };
}

function newchoiceplateau(myPlateau) {
    var i, j;
    for(i = 0; i<myPlateau.hauteur; i++){
        for(j = 0; j<myPlateau.largeur; j++){
            Plateau_Graphique_mycoloring(i, j, initialcolor, myPlateau);
        }};
}

function Plateau_Graphique_preset (){
    var x, y, newplateau, nb;
    if(verrouplateau == 1){ alert(message[0][language]); return;};

    switch (document.getElementById("Preset").value){
    case "c1": newplateau = Shape_Anglais; break;
    case "c2": newplateau = Shape_Francais; break;
    case "c3": newplateau = Shape_Grand; break;
    case "c4": newplateau = Shape_Cour_Carre; break;
    case "c5": newplateau = Shape_Patio; break;
    case "c6": newplateau = Shape_Wiegleb; break;
    case "c7": newplateau = Shape_Cellules; break;
    default :  newplateau = Shape_Maximal; break;
    }; 
    if(newplateau == Shape_Maximal){return;};

    largeurmaximale = newplateau.largeur;
    hauteurmaximale = newplateau.hauteur;
    nb = -1;
    for(x = 0; x < PlateauMaximal.hauteur; x++){
        for(y = 0; y < PlateauMaximal.largeur; y++){
             if((x < newplateau.hauteur)
                 &(y < newplateau.largeur)){
                 nb++;
                if(newplateau.plateau[nb] == 9){
                    PlateauMaximal.Table[PlateauMaximal.xytocode(x, y)] = 9;
                    Plateau_Graphique_mycoloring(x, y, crossedoutcolor, PlateauMaximal);
                } else {
                    PlateauMaximal.Table[PlateauMaximal.xytocode(x, y)] = 0;
                    Plateau_Graphique_mycoloring(x, y, initialcolor, PlateauMaximal);
                }
             } else {
                 PlateauMaximal.Table[PlateauMaximal.xytocode(x, y)] = 9;
                 Plateau_Graphique_mycoloring(x, y, crossedoutcolor, PlateauMaximal);
             };
        }};
}

function Plateau_Graphique_ote_colonne(){
    var x;
    if(verrouplateau == 1){ alert(message[0][language]); return;};

    if(largeurmaximale > 1){
        for(x = 0; x < PlateauMaximal.hauteur; x++){
            PlateauMaximal.Table[PlateauMaximal.xytocode(x, largeurmaximale-1)] = 9;
            Plateau_Graphique_mycoloring(x, largeurmaximale-1, crossedoutcolor, PlateauMaximal);
        };
        largeurmaximale--;
    };
}

function Plateau_Graphique_ajoute_colonne(){
    var x;
    if(verrouplateau == 1){ alert(message[language][0]); return;};

    if(largeurmaximale < PlateauMaximal.largeur){
        for(x = 0; x < PlateauMaximal.hauteur; x++){
            PlateauMaximal.Table[PlateauMaximal.xytocode(x, largeurmaximale)] = 0;
            Plateau_Graphique_mycoloring(x, largeurmaximale, initialcolor, PlateauMaximal);
        };
        largeurmaximale++;
        hauteurmaximale = PlateauMaximal.hauteur;
    };
    // alert("Nouvelle largeur = "+largeurmaximale);
}

function Plateau_Graphique_ote_ligne(){
    var y;
    if(verrouplateau == 1){ alert(message[0][language]); return;};
    //alert("hauteurmaximale = "+hauteurmaximale);
    if(hauteurmaximale > 1){
        for(y = 0; y < PlateauMaximal.largeur; y++){
            PlateauMaximal.Table[PlateauMaximal.xytocode(hauteurmaximale-1, y)] = 9;
            Plateau_Graphique_mycoloring(hauteurmaximale-1, y, crossedoutcolor, PlateauMaximal);
        };
        hauteurmaximale--;
    };
}

function Plateau_Graphique_ajoute_ligne(){
    var y;
    if(verrouplateau == 1){ alert(message[0][language]); return;};

    if(hauteurmaximale < PlateauMaximal.hauteur){
        for(y = 0; y < PlateauMaximal.largeur; y++){
            PlateauMaximal.Table[PlateauMaximal.xytocode(hauteurmaximale, y)] = 0;
            Plateau_Graphique_mycoloring(hauteurmaximale, y, initialcolor, PlateauMaximal);
        };
        hauteurmaximale++;
        largeurmaximale = PlateauMaximal.largeur;
    };
}

function Plateau_Graphique_toggle_verrou(){
    if(verrouplateau == 1){
        verrouplateau = 0;
        //alert(document.getElementById("VerrouPlateau").value);
        document.getElementById("etude_position_initiale").innerHTML 
            = "<font style=\"font-weight:bold;font-size:300%\">?</font>";
        document.getElementById("etude_position_finale").innerHTML 
            = "<font style=\"font-weight:bold;font-size:300%\">?</font>";
        document.getElementById("VerrouPlateau").value=message[1][language];
        document.getElementById("display_carac").innerHTML = "";
        document.getElementById("Caracteristiques").innerHTML = "";
        document.getElementById("display_pot_elem").innerHTML = "";
        document.getElementById("Pot_Elem").innerHTML = "";
        document.getElementById("ShowTab").innerHTML = "";
        document.getElementById("nbcoups").innerHTML = "";
        document.getElementById("nbcases").innerHTML = "";
        document.getElementById("nbcarac").innerHTML = "";
        document.getElementById("compare_carac").innerHTML = "";
        document.getElementById("compared_carac").innerHTML = "";
        document.getElementById("compare_pot_elem").innerHTML = "";
        document.getElementById("compared_pot_elem").innerHTML = "";
        document.getElementById("cleaner").innerHTML = "";
        document.getElementById("vide_ini").innerHTML = "";
        document.getElementById("plein_ini").innerHTML = "";
        document.getElementById("deriver").innerHTML = "";
        document.getElementById("deriverviajava").innerHTML = "";
        document.getElementById("derived").innerHTML = "";
        document.getElementById("derivedviajava").innerHTML = "";
        document.getElementById("display_derivations").innerHTML = "";
        document.getElementById("derivations").innerHTML = "";
        document.getElementById("play_derivation").innerHTML = ""; 
        document.getElementById("show_path").innerHTML = "";
        document.getElementById("restore_ini").innerHTML = ""; 
    } else {
        verrouplateau = 1;
        document.getElementById("VerrouPlateau").value=message[2][language];
    }
}

function Plateau_Graphique_set_plateau(){
    var x, y, table = new Array();
    var hauteurmax = 0;
    var largeurmax = 0;
    var hauteurmin = PlateauMaximal.hauteur;
    var largeurmin = PlateauMaximal.largeur;
    if(verrouplateau == 0){ Plateau_Graphique_toggle_verrou();};

    for(x = 0; x< PlateauMaximal.hauteur; x++){
        for(y = 0; y< PlateauMaximal.largeur; y++){
            if(PlateauMaximal.Table[PlateauMaximal.xytocode(x,y)] != 9){
                hauteurmax = mymax(hauteurmax, x+1);
                largeurmax = mymax(largeurmax, y+1);
                hauteurmin = mymin(hauteurmin, x);
                largeurmin = mymin(largeurmin, y);
            };};};
 
    PlateauEtude = new PlateauSolitaire(hauteurmax-hauteurmin, largeurmax-largeurmin, 
                                        table, "Etude_Graphique");
    for(x = 0; x< PlateauEtude.hauteur; x++){
        for(y = 0; y< PlateauEtude.largeur; y++){
            PlateauEtude.Table[PlateauEtude.xytocode(x,y)] 
                = PlateauMaximal.Table[PlateauMaximal.xytocode(x,y)];}};
 
    PlateauEtude.Coups = CalculeCoups(PlateauEtude);
    document.getElementById("nbcoups").innerHTML = 
        "   " + PlateauEtude.Coups.length + message[3][language];
    PlateauEtude.nbcases = Calculenbcases(PlateauEtude);
    document.getElementById("nbcases").innerHTML = 
        message[4][language] + PlateauEtude.nbcases + message[9][language]+",";
    
    PlateauEtude.liste_carac = create_caracteristiques (PlateauEtude);
    PlateauEtude.base_hash = choose_base_hash(PlateauEtude);
    PlateauEtude.liste_pot_elem = create_potentiels_elementaires (PlateauEtude);
    document.getElementById("nbcarac").innerHTML = 
        "   " + message[5][language] + PlateauEtude.liste_carac.length + " "+ message[6][language];
    // Il faut fermer ces boutons au niveau de Plateau_Graphique_toggle_verrou
    no_display_caracteristiques();
    no_display_pot_elem();

    /* Tells JAVA about it !! */
    //alert("Going to java, yo! ");
    // Le plateau :
    //document.JavaLibrary.initPlateauEtude();

    document.getElementById("ShowTab").innerHTML = 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
        +"onClick=\"TableauDeBord()\" value=\""
        +message[7][language]+ "\">";

    document.getElementById("compare_carac").innerHTML =
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
        +" onClick=\"window.compare_caracteristiques(Etude, PlateauEtude)\" value=\""+message[8][language] +"\">";
    document.getElementById("compare_pot_elem").innerHTML =
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
        +" onClick=\"window.compare_pot_elem(Etude, PlateauEtude)\" value=\""+message[10][language]+"\">";
    document.getElementById("deriver").innerHTML =
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
        +" onClick=\"window.essaie_de_deriver(Etude, PlateauEtude)\" value=\""+message[11][language]+"\">";
    document.getElementById("deriverviajava").innerHTML =
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
        +" onClick=\"window.essaie_de_deriver_via_java(Etude, PlateauEtude)\" value=\""+message[26][language]+"\">";

    document.getElementById("derivations").innerHTML = "";
    document.getElementById("display_derivations").innerHTML = "";

    document.getElementById("cleaner").innerHTML = "<input type=\"button\""
        +" style=\"width:100px;background-color:#FFFFCC\""
        +" onClick=\"window.cleaner()\" value=\""+message[12][language]+"\">"; //alert("Yo");
    document.getElementById("vide_ini").innerHTML = "<input type=\"button\""
        +" style=\"width:100px;background-color:#FFFFCC\""
        +" onClick=\"window.Vide_etude_initiale()\" value=\""+message[13][language]+"\">"; 
    document.getElementById("plein_ini").innerHTML = "<input type=\"button\""
        +" style=\"width:100px;background-color:#FFFFCC\""
        +" onClick=\"window.Remplis_etude_initiale()\" value=\""+message[14][language]+"\">";

    document.getElementById("play_derivation").innerHTML = ""; 
    document.getElementById("restore_ini").innerHTML = "";
    document.getElementById("show_path").innerHTML = "";

    Set_Etude_Graphique_ini();
    Etude_Graphique_display_position_initiale(); 
    Etude_Graphique_display_position_finale();

}

function Set_Etude_Graphique_ini(){
    var x, y, newHTML = "", pos_ini, pos_fin;

    pos_ini = new Array(PlateauEtude.hauteur * PlateauEtude.largeur);
    pos_fin = new Array(PlateauEtude.hauteur * PlateauEtude.largeur);
    Etude = new uneEtude(pos_ini, pos_fin);
    for(x = 0; x < PlateauEtude.hauteur; x++){
        newHTML = newHTML + "<tr>";
        for(y = 0; y < PlateauEtude.largeur; y++){
            Etude.position_initiale[PlateauEtude.xytocode(x,y)] = 9;
            if(PlateauEtude.Table[PlateauEtude.xytocode(x,y)]!=9){
                newHTML = newHTML + "<td><input type=\"button\""
                    +" id=\"Etude_Graphique_button_"+lettres[x]+lettres[y]+"\" value=\""
                    +valuepoint+"\" onClick=\"window.Etude_Graphique_select("+x+","+y+")\"></td>";
                Etude.position_initiale[PlateauEtude.xytocode(x,y)] = 1;
            } else {
                newHTML = newHTML + "<td></td>";
                Etude.position_initiale[PlateauEtude.xytocode(x,y)] = 9;
            };};
        newHTML = newHTML + "</tr>";
    };
    document.getElementById("etude_position_initiale").innerHTML = newHTML;

    newHTML = "";
    for(x = 0; x < PlateauEtude.hauteur; x++){
        newHTML = newHTML + "<tr>";
        for(y = 0; y < PlateauEtude.largeur; y++){
            Etude.position_finale[PlateauEtude.xytocode(x,y)] = 9;
            if(PlateauEtude.Table[PlateauEtude.xytocode(x,y)]!=9){
                newHTML = newHTML + "<td><input type=\"button\""
                    +" id=\"Etude_Graphique_final_button_"+lettres[x]+lettres[y]+"\" value=\""
                    +valuenix+"\" onClick=\"window.Etude_Graphique_final_select("+x+","+y+",PlateauEtude)\"></td>";
                 Etude.position_finale[PlateauEtude.xytocode(x,y)] = 0;
           } else {
                newHTML = newHTML + "<td></td>";
                Etude.position_finale[PlateauEtude.xytocode(x,y)] = 9;
            };};
        newHTML = newHTML + "</tr>";
    };
    document.getElementById("etude_position_finale").innerHTML = newHTML;
}

function Etude_Graphique_display_position_initiale(){
    Jeu_Graphique_display_final(PlateauEtude, Etude.position_initiale, "", 1);
}

function Etude_Graphique_display_position_finale(){
    Jeu_Graphique_display_final(PlateauEtude, Etude.position_finale, "_final", 1);
}

function Etude_Graphique_select (x, y){
    var bouton = PlateauEtude.name+"_button_"+lettres[x]+lettres[y];
    prepare_ground(); // Ok, we do too many things --
    if (Etude.position_initiale[PlateauEtude.xytocode(x, y)] == 1){
        Etude.position_initiale[PlateauEtude.xytocode(x, y)] = 0;
        document.getElementById(bouton).value = "      ";
    } else if (Etude.position_initiale[PlateauEtude.xytocode(x, y)] == 0){
        Etude.position_initiale[PlateauEtude.xytocode(x, y)] = 1;
        document.getElementById(bouton).value = "  o  ";
    };
}

function Etude_Graphique_final_select (x, y){
    var bouton = PlateauEtude.name+"_final_button_"+lettres[x]+lettres[y];
    prepare_ground(); // Ok, we do too many things --
    if (Etude.position_finale[PlateauEtude.xytocode(x, y)] == 1){
        Etude.position_finale[PlateauEtude.xytocode(x, y)] = 0;
        document.getElementById(bouton).value = "      ";
    } else if (Etude.position_finale[PlateauEtude.xytocode(x, y)] == 0){
        Etude.position_finale[PlateauEtude.xytocode(x, y)] = 1;
        document.getElementById(bouton).value = "  o  ";
    };
}

function Jeu_Graphique_myvalue(x, y, myvalue, myPlateau){
    document.getElementById(myPlateau.name+"_button_"+lettres[x]+lettres[y]).value=myvalue;
}

function Vide_etude_initiale(){
    var x, y;

    prepare_ground();
    for(x = 0; x < PlateauEtude.hauteur; x++){
        for(y = 0; y < PlateauEtude.largeur; y++){
            if(PlateauEtude.Table[PlateauEtude.xytocode(x, y)]!=9){
                //alert(lettres[x]+lettres[y]);
                Etude.position_initiale[PlateauEtude.xytocode(x, y)] = 0;
                Jeu_Graphique_myvalue(x, y, valuenix, PlateauEtude);}}};
}

function Remplis_etude_initiale(){
    var x, y;

    prepare_ground();
    for(x = 0; x < PlateauEtude.hauteur; x++){
        for(y = 0; y < PlateauEtude.largeur; y++){
            if(PlateauEtude.Table[PlateauEtude.xytocode(x, y)]!=9){
                Etude.position_initiale[PlateauEtude.xytocode(x, y)] = 1;
                Jeu_Graphique_myvalue(x, y, valuepoint, PlateauEtude);}}};
}

function restore_ini(){
    var x, y;

    for(x = 0; x < PlateauEtude.hauteur; x++){
        for(y = 0; y < PlateauEtude.largeur; y++){
            if(PlateauEtude.Table[PlateauEtude.xytocode(x, y)]!=9){
                if(Etude.position_initiale[PlateauEtude.xytocode(x, y)] == 1){
                    Jeu_Graphique_myvalue(x, y, valuepoint, PlateauEtude);
                } else {
                    Jeu_Graphique_myvalue(x, y, valuenix, PlateauEtude);
                };}}};
}

function cleaner(){
    Remplis_etude_initiale();
    Etude.derivations = [];
    document.getElementById("compared_carac").innerHTML = ""; 
    document.getElementById("compared_pot_elem").innerHTML = ""; 
    document.getElementById("derived").innerHTML = ""; 
    document.getElementById("derivedviajava").innerHTML = "";

    document.getElementById("display_derivations").innerHTML = "";
    document.getElementById("derivations").innerHTML = "";
    document.getElementById("play_derivation").innerHTML = ""; 
    document.getElementById("restore_ini").innerHTML = ""; 
    document.getElementById("show_path").innerHTML = "";
}


<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->

function prod_scalaire (pos1, pos2, myPlateau){
    var x, y, co, res = 0;  
    
    for(x=0; x<myPlateau.hauteur; x++){
        for(y=0; y<myPlateau.largeur; y++){
            co = myPlateau.xytocode(x,y);
            if(myPlateau.Table[co] != 9){ res += pos1[co]*pos2[co];}}};
    return res;
};

<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->
