/* Caracteristiques.js : un programme pour etudier le solitaire
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

function value_from_neighbour(xP,yP,xQ,yQ,xR,yR, carac, myPlateau){
    if((xP>=0)&(xP<myPlateau.hauteur)
       &(xQ>=0)&(xQ<myPlateau.hauteur)
       &(xR>=0)&(xR<myPlateau.hauteur)
       &(yP>=0)&(yP<myPlateau.largeur)
       &(yQ>=0)&(yQ<myPlateau.largeur)
       &(yR>=0)&(yR<myPlateau.largeur)
       &(myPlateau.Table[myPlateau.xytocode(xP,yP)]!=9)
       &(myPlateau.Table[myPlateau.xytocode(xQ,yQ)]!=9)
       &(myPlateau.Table[myPlateau.xytocode(xR,yR)]!=9)
      ){
        if ((carac[myPlateau.xytocode(xP,yP)]!=-1)
            &(carac[myPlateau.xytocode(xQ,yQ)]!=-1)){
            return((carac[myPlateau.xytocode(xP,yP)]+carac[myPlateau.xytocode(xQ,yQ)])%2);
        } else if ((carac[myPlateau.xytocode(xP,yP)]!=-1)
                   &(carac[myPlateau.xytocode(xR,yR)]!=-1)){
            return((carac[myPlateau.xytocode(xP,yP)]+carac[myPlateau.xytocode(xR,yR)])%2);
        } else if ((carac[myPlateau.xytocode(xQ,yQ)]!=-1)
                   &(carac[myPlateau.xytocode(xR,yR)]!=-1)){
            return((carac[myPlateau.xytocode(xQ,yQ)]+carac[myPlateau.xytocode(xR,yR)])%2);
        } else { return -1;};
    } else { return -1;};
}

function complete_caracteristique (carac, myPlateau) {
    /* carac est une position avec des 0, des 1 et des -1
     * On essaie de déduire la valeur des cases avec -1 en fonction des autres.
     * Si on reussit a completer une case, on continue
     * A la fin, on renvoit le tableau carac */
    var docontinue = 1, alldone = 0, res1, res2, res3, res4, res5, res6;
    var x, y, allres, jusquou, newcarac;
    /* incompatibility --> alldone = -1
     * over --> alldone = -1
     * a square remains undecided --> alldone = 0 
     * */
    while((docontinue == 1)&(alldone == 0)){
        alldone = 1;
        docontinue = 0;
        for(x = 0; x < myPlateau.hauteur; x++){
            for(y = 0; y < myPlateau.largeur; y++){
                if (carac[myPlateau.xytocode(x,y)] == -1){
                    res1 = value_from_neighbour( x-2, y, x-1, y, x, y, carac, myPlateau);
                    res2 = value_from_neighbour( x+2, y, x+1, y, x, y, carac, myPlateau);
                    res3 = value_from_neighbour( x, y-2, x, y-1, x, y, carac, myPlateau);
                    res4 = value_from_neighbour( x, y+2, x, y+1, x, y, carac, myPlateau);
                    res5 = value_from_neighbour( x+1, y, x, y, x-1, y, carac, myPlateau);
                    res6 = value_from_neighbour( x, y+1, x, y, x, y-1, carac, myPlateau);
                    allres = [ res1, res2, res3, res4, res5, res6];
                    allres.sort(function(a,b){return b - a}); /* Le plus grand au debut */
                    //if(allres[0]<allres[1]){ alert("??");};
                    if(allres[0] == -1){
                        /* No way to complete: */
                        alldone = 0;
                    } else {
                        jusquou = 0;
                        while(allres[jusquou] != -1){jusquou++;};
                        jusquou--;
                        if(allres[0] != allres[jusquou] ){
                            //alert("Impossible = "+allres);
                            return -1; /* incompability */
                        } else {
                            carac[myPlateau.xytocode(x,y)] = allres[0];
                            docontinue = 1;
                        };
                    };
                };
                // if alldone == 0 --> one square is not filled
                // if docontinue == 1  --> one square has been filled.
            };};
    };
    //alert("complete_caracteristique : "+alldone);
    return([alldone, carac]);
}


function Populated_position(val, myPlateau){
    var x, y, res = new Array(myPlateau.largeur*myPlateau.hauteur);
    for(x=0; x < myPlateau.hauteur; x++){
        for(y=0; y < myPlateau.largeur; y++){
            if(myPlateau.Table[myPlateau.xytocode(x,y)]==9){
                res[myPlateau.xytocode(x,y)] = 9;
            } else {
                res[myPlateau.xytocode(x,y)] = val;
            }}};
    return(res);
}

function find_undecided_square(position, myPlateau){
    var y, x = 0;
    // alert("Oh "+position);
    while(x < myPlateau.hauteur){
        y = 0;
        while(y < myPlateau.largeur){
            if(position[myPlateau.xytocode(x,y)] == -1){
                return([x,y]);
            };
            y++;};
        x++;};
    return([-1,-1]); /* None found !!!*/
}


function create_caracteristiques_with_ini (beg_carac, myPlateau) {
    var carac, beg, res1, res2, aux1, aux2;

    /* Choose entry point: */
    beg = find_undecided_square(beg_carac, myPlateau);
    if(beg[0]==-1){return [];};

    /* Create a copy: */
    carac = beg_carac.slice(0);
    //alert("carac = "+carac);

    beg_carac[myPlateau.xytocode(beg[0], beg[1])] = 0;
    res1 = complete_caracteristique(beg_carac, myPlateau);
    //alert(res1);
    /* res1 = -1 --> contradiction 
     * res1 = [1, position] --> une position et c'est la fin 
     * res2 = [0, position] --> une position et on peut continuer */

    carac[myPlateau.xytocode(beg[0], beg[1])] = 1;
    res2 = complete_caracteristique(carac, myPlateau);
    /* voir la note pour res1 */
    if(res1==-1){
        if(res2==-1){
            return [];
        } else if (res2[0]==1){
            return([res2[1]]);
        } else {
            return(create_caracteristiques_with_ini(res2[1], myPlateau));
        }
    } else if(res1[0]==1){
        if(res2==-1){
            return [res1[1]];
        } else if (res2[0]==1){
            return([res1[1], res2[1]]);
        } else {
            return([res1[1]].concat(create_caracteristiques_with_ini(res2[1], myPlateau)));
        }
    } else {
        if(res2==-1){
            return(create_caracteristiques_with_ini(res1[1], myPlateau));
        } else if (res2[0]==1){
            aux1 = create_caracteristiques_with_ini(res1[1], myPlateau);
            aux1.concat([res2[1]]);
            //alert(aux1.length+ " ");
            return(aux1.concat([res2[1]]));
        } else {
            aux1 = create_caracteristiques_with_ini(res1[1], myPlateau);
            aux2 = create_caracteristiques_with_ini(res2[1], myPlateau);
            //alert(aux1.length+ " "+aux2.length);
            return(aux1.concat(aux2));
        }
    };
};

function equal_pos(pos1, pos2){
    var x=0;
    while(x< pos1.length){
        if(pos1[x] != pos2[x]){return 0;};
        x++;};
    return 1;
};

function pos_member(pos, liste_pos){
    var x = 0;
    while(x<liste_pos.length){
        if(equal_pos(pos, liste_pos[x])){return 1;};
        x++;};
    return 0;
}

function add_pos_mod_2(pos1, pos2){
    var x, newpos = new Array(pos1.length);
    for(x=0; x<pos1.length; x++){
        if(pos1[x]==9){
            newpos[x]=9;
        } else {
            newpos[x] =(pos1[x]+pos2[x])%2;
        }};
    return newpos;
}

function extend_space(pos, liste_pos){
    var x, ll = liste_pos.length;
    for(x=0; x < ll; x++){
        // alert("x = "+x+" Au debut length = "+ll+" et a la fin length = "+liste_pos.length);
        liste_pos.push(add_pos_mod_2 (liste_pos[x], pos));};
    //alert("Au debut length = "+ll+" et a la fin length = "+liste_pos.length);
    return liste_pos;
}

function create_caracteristiques (myPlateau) {
    var x, y, carac, res, dimension, aux, go, isnonzero;
    var freefamily, generatedfamily, auxfamily;

    /* Choose entry point: */
    carac = Populated_position(-1, myPlateau);
    res = create_caracteristiques_with_ini (carac, myPlateau);
    /* Maintenant res contient toutes les caracteristiques, mais
     * certaines sont lineairement dependantes et 
     * res.length est une puissance de 2 (16 en general).*/
    //alert("nb caracteristiques "+res.length);
    dimension = 0;
    aux = 1;
    while(aux < res.length){aux *= 2; dimension++;};
    //alert("Dimension = "+dimension);


    /* Un des elements de res est 0. On l'enleve. */
    aux = 0; go = 1;
    while(go == 1){
        isnonzero = 0;
        for(x=0; x<myPlateau.hauteur; x++){
            for(y=0; y<myPlateau.largeur; y++){
                if(res[aux][myPlateau.xytocode(x,y)]==1){isnonzero = 1;};}};
        if(isnonzero == 1){ aux++; } else { go = 0; }};
    generatedfamily = [res[aux]];
    res.splice(aux, 1);

    freefamily = [res[0]];
    generatedfamily.push(res[0]);
    res.splice(0, 1);// alert("Size of space = "+generatedfamily.length);
    
    while(freefamily.length<dimension){
        if(pos_member(res[0], generatedfamily)){
            res.splice(0, 1);
        } else {
            freefamily.push(res[0]);
            generatedfamily = extend_space(res[0], generatedfamily);
            //alert("Size of space = "+generatedfamily.length);
            res.splice(0, 1);}};

    return(freefamily);
};

function display_caracteristiques(myPlateau) {
    var c, x, y, tt, newHTML="";
    
    newHTML += '<table class="jeu"><tr>';
    for(c=0; c< myPlateau.liste_carac.length; c++){
        newHTML += " <td><table>";
        for(x=0; x< myPlateau.hauteur; x++){
         newHTML += "<tr>";
           for(y=0; y< myPlateau.largeur; y++){
                tt = myPlateau.liste_carac[c][myPlateau.xytocode(x,y)];
                if(tt == 9){
                    newHTML += "<td> </td>";
                } else if (tt == 1){
                    newHTML += "<td style=\"width:15px;background-color:#C0C0C0;text-align:center\">1</td>";
                } else if (tt == 0){
                    newHTML += "<td style=\"width:15px;background-color:#FFFF66;text-align:center\">0</td>";
                } else {
                    newHTML += "<td style=\"width:15px;background-color:red;text-align:center\">"+tt+"</td>";
                    ;};};
            newHTML += "</tr>";
        };
        newHTML += "</table></td><td>&nbsp;&nbsp;&nbsp;</td>";
    };
    newHTML += "</tr></table>";
    document.getElementById("display_carac").innerHTML = newHTML;
    document.getElementById("Caracteristiques").innerHTML = 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\" onClick=\"window.no_display_caracteristiques()\" value=\"Cacher les caracteristiques\">";
};

function no_display_caracteristiques() {
    
    document.getElementById("display_carac").innerHTML = "";
    document.getElementById("Caracteristiques").innerHTML = 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\" onClick=\"window.display_caracteristiques(PlateauEtude)\" value=\"Montrer les caracteristiques\">";
};

function res_delta_mod_deux(carac, myJeu, myPlateau){
    var res;

    res = (prod_scalaire(carac, myJeu.position_finale, myPlateau)
           -prod_scalaire(carac, myJeu.position_initiale, myPlateau))%2;
    if(res < 0){res += 2;};
    return res;
}

function compare_caracteristiques(myJeu, myPlateau){
    var carac, res, c, allres;

    document.getElementById("compared_carac").innerHTML =
        "<img src=\"../../trucs/arrow-jazzy-right.jpg\">&nbsp; ";
    allres = new Array(myPlateau.liste_carac.length);
    for(c = 0; c < myPlateau.liste_carac.length; c++){
        carac = myPlateau.liste_carac[c];
        res = (prod_scalaire(carac, myJeu.position_finale, myPlateau)
               -prod_scalaire(carac, myJeu.position_initiale, myPlateau))%2;
        if(res < 0){res += 2;};
        document.getElementById("compared_carac").innerHTML += res + "&nbsp; ";
        allres[c]=res;};
    return allres;
}