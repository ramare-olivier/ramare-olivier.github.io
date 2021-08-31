/* Potentiels.js : un programme pour etudier le solitaire
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

/* Les potentiels type Fibonnaci : */


function create_pot_elem_with_all (lig, col, myPlateau) {
    var x, y, pos;
    
    pos = new Array(myPlateau.hauteur*myPlateau.largeur);
    //alert(lig+"\n"+col);
    for(x = 0; x<myPlateau.hauteur; x++){
       for(y = 0; y<myPlateau.largeur; y++){
           pos[myPlateau.xytocode(x,y)] = lig[y]*col[x];}};
    return pos;
}

function create_pot_elem_with_inipp (h1, h2, v1, v2, myPlateau) {
    var x, y, lignebase, colonnebase;

    lignebase = new Array(myPlateau.largeur);
    colonnebase = new Array(myPlateau.hauteur);

    lignebase[0] = h1;
    if(myPlateau.largeur>0){
        lignebase[1] = h2;
        for(y=2; y<myPlateau.largeur; y++){
            lignebase[y] = lignebase[y-1] + lignebase[y-2];}};

    colonnebase[0] = v1;
    if(myPlateau.hauteur>0){
        colonnebase[1] = v2;
        for(x = 2; x < myPlateau.hauteur; x++){
            colonnebase[x] = colonnebase[x-1] + colonnebase[x-2];}};

    return create_pot_elem_with_all (lignebase, colonnebase, myPlateau);
}

function invert_potmp ( pot, myPlateau){
    var x, y, pos = new Array(myPlateau.hauteur*myPlateau.largeur);
    for(x = 0; x < myPlateau.hauteur; x++){
       for(y = 0; y < myPlateau.largeur; y++){
           pos[myPlateau.xytocode( myPlateau.hauteur-x-1, y)] = 
               pot[myPlateau.xytocode( x, y)];}};
    return pos;
}

function invert_potpm ( pot, myPlateau){
    var x, y, pos = new Array(myPlateau.hauteur*myPlateau.largeur);
    for(x = 0; x<myPlateau.hauteur; x++){
       for(y = 0; y<myPlateau.largeur; y++){
           pos[myPlateau.xytocode(x,myPlateau.largeur-y-1)] = 
               pot[myPlateau.xytocode(x,y)];}};
    return pos;
}

function invert_potmm ( pot, myPlateau){
    var x, y, pos = new Array(myPlateau.hauteur*myPlateau.largeur);
    for(x = 0; x<myPlateau.hauteur; x++){
       for(y = 0; y<myPlateau.largeur; y++){
           pos[myPlateau.xytocode(myPlateau.hauteur-x-1,myPlateau.largeur-y-1)] = 
               pot[myPlateau.xytocode(x,y)];}};
    return pos;
}

/* Les potentiels de comptages : */

function create_pot_elem_comptage (a1, a2, b1, b2, myPlateau) {
    var x, y, pot = new Array(myPlateau.hauteur*myPlateau.largeur);
    for(x = 0; x < myPlateau.hauteur; x++){
        for(y = 0; y < myPlateau.largeur; y++){
            if(((x%2)==0)&((y%2)==0)){
                pot[myPlateau.xytocode(x,y)] = a1;
            } else if (((x%2)==1)&((y%2)==0)){
                pot[myPlateau.xytocode(x,y)] = a2;
            } else if (((x%2)==0)&((y%2)==1)){
                pot[myPlateau.xytocode(x,y)] = b1;
            } else {
                pot[myPlateau.xytocode(x,y)] = b2;}}};
    return pot;
}


/*-------------------------------------------------------*/

function create_potentiels_elementaires (myPlateau) {
    var x, y, pot, alist;

    pot = create_pot_elem_with_inipp(1,0,1,0, myPlateau);
    alist = new Array(pot); 
    alist.push(create_pot_elem_with_inipp(1,0,0,1, myPlateau));
    alist.push(create_pot_elem_with_inipp(0,1,1,0, myPlateau));
    alist.push(create_pot_elem_with_inipp(0,1,0,1, myPlateau));

    alist.push(invert_potmp(alist[0], myPlateau));
    alist.push(invert_potmp(alist[1], myPlateau));
    alist.push(invert_potmp(alist[2], myPlateau));
    alist.push(invert_potmp(alist[3], myPlateau));

    alist.push(invert_potpm(alist[0], myPlateau));
    alist.push(invert_potpm(alist[1], myPlateau));
    alist.push(invert_potpm(alist[2], myPlateau));
    alist.push(invert_potpm(alist[3], myPlateau));

    alist.push(invert_potmm(alist[0], myPlateau));
    alist.push(invert_potmm(alist[1], myPlateau));
    alist.push(invert_potmm(alist[2], myPlateau));
    alist.push(invert_potmm(alist[3], myPlateau));

    alist.push(create_pot_elem_comptage(1, 0, 0, 0, myPlateau));
    alist.push(create_pot_elem_comptage(0, 1, 0, 0, myPlateau));
    alist.push(create_pot_elem_comptage(0, 0, 1, 0, myPlateau));
    alist.push(create_pot_elem_comptage(0, 0, 0, 1, myPlateau));
    return alist;
}

function no_display_pot_elem() {
    
    document.getElementById("display_pot_elem").innerHTML = "";
    document.getElementById("Pot_Elem").innerHTML = 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\" onClick=\"window.display_pot_elem(PlateauEtude)\" value=\"Montrer les potentiels elementaires\">";
};

function display_pot_elem(myPlateau) {
   var c, x, y, tt, newHTML="", nb=0;
    
    newHTML += '<table class="jeu"><tr>';
    for(c = 0; c < myPlateau.liste_pot_elem.length; c++){
        newHTML += " <td><table>";
        for(x = 0; x< myPlateau.hauteur; x++){
         newHTML += "<tr>";
           for(y = 0; y< myPlateau.largeur; y++){
               if(myPlateau.Table[myPlateau.xytocode(x,y)] != 9){
                   tt = PlateauEtude.liste_pot_elem[c][myPlateau.xytocode(x,y)];
                   newHTML += "<td style=\"width:35px;background-color:#CCCC33;text-align:center\">"+tt+"</td>";
               } else {
                   newHTML += "<td> </td>";
               };};
            newHTML += "</tr>";
        };
        nb++;
        if(nb % nb_derivations_displayed_per_line == 0){
            newHTML += "</table></td></tr><tr>";
        } else {
            newHTML += "</table></td><td>&nbsp;&nbsp;&nbsp;</td>";
        }};
    newHTML += "</tr></table>";
    document.getElementById("display_pot_elem").innerHTML = newHTML;
    document.getElementById("Pot_Elem").innerHTML = 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\" onClick=\"window.no_display_pot_elem()\" value=\"Cacher les potentiels elementaires\">";
};

/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/

function compare_pot_elem(myJeu, myPlateau){
    var pot_elem, res, c, allres = [];

    document.getElementById("compared_pot_elem").innerHTML =
        "<img src=\"../../trucs/arrow-jazzy-right.jpg\">&nbsp; ";
    for(c = 0; c < myPlateau.liste_pot_elem.length; c++){
        pot_elem = myPlateau.liste_pot_elem[c];
        res = score_pot(myJeu.position_initiale, myJeu.position_finale, pot_elem, myPlateau);
        document.getElementById("compared_pot_elem").innerHTML += res + "&nbsp; ";
        allres.push(res);};
    return allres;
}

/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/



