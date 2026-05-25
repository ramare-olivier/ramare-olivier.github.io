/* TableauDeBord.js : un programme pour etudier le solitaire
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

var Cockpit; // la fenetre du tableau de bord
var PegAwacs; // le fenetre principale. Ne fonctionne pas :(
var longueur_pas_descente, max_descente, remonter_de_tant, language, message, nb_derivations_displayed_per_line;
var show_time_one, show_time_two, show_time_three, show_branches;

function includeJS(jsPath){
  var js = document.createElement("script");
  js.setAttribute("type", "text/javascript");
  js.setAttribute("src", jsPath);
  document.getElementsByTagName("head")[0].appendChild(js);
}
<!-- -------------------------------------- -->
includeJS("Messager.js");
<!-- -------------------------------------- -->

function TableauDeBord (){
    Cockpit = window.open("TableauDeBord.html", "TableauDeBord", 
                          "height=700,width=500,resizable=yes,scrollbar=yes");
    Cockpit.PlateauEtude = PlateauEtude;
    Cockpit.longueur_pas_descente = longueur_pas_descente;
    Cockpit.max_descente = max_descente;
    Cockpit.remonter_de_tant = remonter_de_tant;
    Cockpit.language = language;
    Cockpit.message = message;
    Cockpit.nb_derivations_displayed_per_line =
        nb_derivations_displayed_per_line;
    Cockpit.show_time_one = show_time_one;
    Cockpit.show_time_two = show_time_two;
    Cockpit.show_time_three = show_time_three;
    Cockpit.show_time_four = show_time_four;
    Cockpit.show_branches = show_branches;
    
}

function contains(a, obj) {
    for (var i = 0; i < a.length; i++) {
        if (a[i] === obj) {return true;} };
    return false;
}

function display_base_hash(){
    var x, y, tt, indice = 0, newHTML="";
    
    newHTML += "<table><tr>";
    newHTML += " <td><table>";
    for(x=0; x< PlateauEtude.hauteur; x++){
        newHTML += "<tr>";
        for(y=0; y< PlateauEtude.largeur; y++){
            tt = PlateauEtude.Table[PlateauEtude.xytocode(x,y)];
            if(tt == 9){
                newHTML += "<td> </td>";
            } else if (contains(PlateauEtude.base_hash, indice)){
                newHTML += "<td style=\"width:15px;background-color:#C0C0C0;text-align:center\">X</td>";
            } else {
                newHTML += "<td style=\"width:15px;background-color:#FFFF66;text-align:center\">0</td>";
            };
            indice++;};
            newHTML += "</tr>";
        };
        newHTML += "</table></td><td>&nbsp;&nbsp;&nbsp;</td>";

    newHTML += "</tr></table>";
    document.getElementById("display_base_hash").innerHTML = newHTML;
    document.getElementById("BaseHash").innerHTML = 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
        +" onClick=\"no_display_base_hash()\" value=\""
        + message[27][language]+ "\">";
};

function no_display_base_hash(){
    
    document.getElementById("display_base_hash").innerHTML = "";
    document.getElementById("BaseHash").innerHTML = 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
        +" onClick=\"display_base_hash()\" value=\""
        + message[28][language]+ "\">";
};

/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function build_for_integer (quidefault, quimoins, quiplus, quiname, optionname, HTMLname, textpresentation){
    var newHTML = "";
        
    newHTML = "<table><tr><td style=\"font-size:9pt;width:300px;background-color:#FFFFCC\">"
        + textpresentation +" :</td>";
    newHTML += "<td><select id=\""+quiname+"\"" +
        "onClick=\"show_"+quiname+" (0)\" >";
    for(var x = quimoins; x <= quiplus; x++){
        if(x == quidefault){
             newHTML += "<option id=\""+optionname+x+"\" selected value=\""+x+"\">"+x+"</option>";
        } else {
            newHTML += "<option id=\""+optionname+x+"\" value=\""+x+"\">"+x+"</option>";}
    };    
    newHTML += "</select></td></tr></table>";
    document.getElementById(HTMLname).innerHTML = newHTML;
}

function build_for_boolean (quidefault, quiname, optionname, HTMLname, textpresentation){
    var newHTML = "";
    var quidefaultstring;
    var choices = ["Oui", "Non"];
    
    if(quidefault) {
         quidefaultstring = choices[0];
    } else {
        quidefaultstring = choices[1];
    }
        
    newHTML = "<table><tr><td style=\"font-size:9pt;width:300px;background-color:#FFFFCC\">"
        + textpresentation +" :</td>";
    newHTML += "<td><select id=\""+quiname+"\"" +
        "onClick=\"show_"+quiname+" (0)\" >";
    for(var x = 0; x <= 1; x++){
        if(choices[x] == quidefaultstring){
             newHTML += "<option id=\""+optionname+x+"\" selected value=\""+choices[x]+"\">"+choices[x]+"</option>";
        } else {
            newHTML += "<option id=\""+optionname+x+"\" value=\""+choices[x]+"\">"+choices[x]+"</option>";}
    };    
    newHTML += "</select></td></tr></table>";
    document.getElementById(HTMLname).innerHTML = newHTML;
}

/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function show_longueur_pas_descente (tellit){
    longueur_pas_descente  = parseInt(document.getElementById("longueur_pas_descente").value);
    if(tellit == 1){alert(message[29][language] + longueur_pas_descente);};
    window.opener.longueur_pas_descente = longueur_pas_descente;
}

function build_longueur_pas_descente (){
    build_for_integer (longueur_pas_descente, 1, 15, "longueur_pas_descente",
                       "pd", "PasDeDescente", "&nbsp; " + message[30][language]);
}

/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function show_nb_derivations_displayed_per_line (tellit){
    nb_derivations_displayed_per_line  = 
        parseInt(document.getElementById("nb_derivations_displayed_per_line").value);
    if(tellit == 1){alert(message[31][language] + " = "+nb_derivations_displayed_per_line);};
    window.opener.nb_derivations_displayed_per_line = nb_derivations_displayed_per_line;
}

function build_nb_derivations_displayed_per_line (){
    build_for_integer (nb_derivations_displayed_per_line, 1, 15, "nb_derivations_displayed_per_line",
                       "nbdd", "NbDerivationsParLigne", "&nbsp; "+ message[31][language]);
}

/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function show_max_descente (tellit){
    max_descente = 
        parseInt(document.getElementById("max_descente").value);
    if(tellit == 1){alert(message[32][language] + " = " + max_descente);};
    window.opener.max_descente = max_descente;
}

function build_max_descente(){
    build_for_integer (max_descente, 1, 70, "max_descente",
                       "pmd", "PMaxDescente", "&nbsp;"+ message[32][language]);
}

/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function show_remonter_de_tant (tellit){
    remonter_de_tant  = parseInt(document.getElementById("remonter_de_tant").value);
    if(tellit == 1){alert(message[33][language] + " = "+remonter_de_tant);};
    window.opener.remonter_de_tant = remonter_de_tant;
}

function build_remonter_de_tant (){
    build_for_integer (remonter_de_tant, 0, 15, "remonter_de_tant",
                       "rdt", "RemonterDeTant", "&nbsp;"+message[34][language]);
}

/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function show_show_time_one (tellit){
    show_time_one  = parseInt(document.getElementById("show_time_one").value);
    if(tellit == 1){alert(message[35][language] + " = "+show_time_one);};
    window.opener.show_time_one = show_time_one;
}

function build_show_time_one (){
    build_for_integer (show_time_one, 1, 15, "show_time_one",
                       "sto", "ShowTimeOne", "&nbsp;"+message[35][language]);
}


/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function show_show_time_two (tellit){
    show_time_two  = parseInt(document.getElementById("show_time_two").value);
    if(tellit == 1){alert(message[36][language] + " = "+show_time_two);};
    window.opener.show_time_two = show_time_two;
}

function build_show_time_two (){
    build_for_integer (show_time_two, 1, 15, "show_time_two",
                       "stt", "ShowTimeTwo", "&nbsp;"+message[36][language]);
}

/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function show_show_time_three (tellit){
    show_time_three  = parseInt(document.getElementById("show_time_three").value);
    if(tellit == 1){alert(message[37][language] + " = "+show_time_three);};
    window.opener.show_time_three = show_time_three;
}

function build_show_time_three (){
    build_for_integer (show_time_three, 1, 15, "show_time_three",
                       "stth", "ShowTimeThree", "&nbsp;"+message[37][language]);
}

/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function show_show_time_four (tellit){
    show_time_four  = parseInt(document.getElementById("show_time_four").value);
    if(tellit == 1){alert(message[44][language] + " = "+show_time_four);};
    window.opener.show_time_four = show_time_four;
}

function build_show_time_four (){
    build_for_integer (show_time_four, 1, 20, "show_time_four",
                       "stf", "ShowTimeFour", "&nbsp;"+message[44][language]);
}


/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

function show_show_branches (tellit){
    var show_branches_str  = document.getElementById("show_branches").value;
    if(tellit == 1){alert(message[29][language] + show_branches_str);};
    if(show_branches_str == "Oui"){
        window.opener.show_branches = show_branches = true;
    } else {
        window.opener.show_branches = show_branches = false;
    }
}

function build_show_branches (){
    build_for_boolean (show_branches, "show_branches",
                       "sb", "ShowBranches", "&nbsp; " + message[46][language]);
}

/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------*/

