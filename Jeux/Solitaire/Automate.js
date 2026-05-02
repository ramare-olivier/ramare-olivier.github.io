/* Automate.js : un programme pour etudier le solitaire
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

var longueur_pas_descente = 2;
var remonter_de_tant = 2;
var show_branches = false;
var nb_derivations_displayed_per_line = 6;
var max_nb_lines_for_derivations = 20;
var max_descente = 40;
var show_time_one = 4; // x 50 to get the time in millisec
var show_time_two = 4; // x 50 to get the time in millisec
var show_time_three = 8; // x 50 to get the time in millisec
var show_time_four = 10; // x 50 to get the time in millisec

/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/

function hashcode(pos, myPlateau) {
    
    return pos[myPlateau.base_hash[0]] + pos[myPlateau.base_hash[1]]*2
        + pos[myPlateau.base_hash[2]]*4 + pos[myPlateau.base_hash[3]]*8
        + pos[myPlateau.base_hash[4]]*16 + pos[myPlateau.base_hash[5]]*32
        + pos[myPlateau.base_hash[6]]*64 + pos[myPlateau.base_hash[7]]*128;
}

function choose_base_hash(myPlateau) {
    var x, y, choice = [], pos=[], indice = 0;
    
    for(x=0; x<myPlateau.hauteur; x++){
        for(y=0; y<myPlateau.largeur; y++){
            if(myPlateau.Table[myPlateau.xytocode(x,y)] != 9){
                pos.push(indice);};
            indice++;}};

    for(x = 0; x < 8 ; x++){ 
        choice.push(pos[Math.min(pos.length-1, Math.round(1+x*pos.length/8))]);};
    /* It is best if the points are not close one to another */
    /* We will do a better choice latter ! */
    //alert(choice);
    return choice;
}

/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/

function joue_coup(pos, coup, myPlateau){
    pos[myPlateau.xytocode(coup.Px, coup.Py)] = 0;
    pos[myPlateau.xytocode(coup.Qx, coup.Qy)] = 0;
    pos[myPlateau.xytocode(coup.Rx, coup.Ry)] = 1;
}

function joue_chemin(pos, chemin, myPlateau){
    var pos_ini = pos.slice(0);
    //alert("Avant : "+pos_ini);
    for(var n=0;n<chemin.length; n++){joue_coup(pos_ini, chemin[n], myPlateau);};
    //alert("Apres : "+pos_ini);
    return pos_ini;
}

function eq_pos(pos1, pos2){
    for(var i = 0; i < pos1.length; i++){
        if(pos1[i] != pos2[i]){return 0;}};
    return 1;
}

function coups_possibles (pos, chemin, myPlateau) {
    /* pos initiale = pos "-" chemin */
    var acoup, liste_coups = [], pos_ini;

    if(chemin.length == 0){
        pos_ini = pos; // chemin est vide
    } else {
        pos_ini = joue_chemin(pos, chemin, myPlateau);};
    //alert("From "+pos_ini);
    for(var c=0; c< myPlateau.Coups.length; c +=2){
        acoup = myPlateau.Coups[c];
          //  alert("Is "+ecrire_coups(acoup)+" here ? "+pos[myPlateau.xytocode(acoup.Qx,acoup.Qy)]);
        if(pos_ini[myPlateau.xytocode(acoup.Qx, acoup.Qy)]==1){
            if((pos_ini[myPlateau.xytocode(acoup.Px, acoup.Py)]==1)
                &&(pos_ini[myPlateau.xytocode(acoup.Rx, acoup.Ry)]==0)){
                //alert("Coup possible "+ecrire_coups(acoup));
                liste_coups.push(acoup);
            } else if ((pos_ini[myPlateau.xytocode(acoup.Px, acoup.Py)]==0)
                       &&(pos_ini[myPlateau.xytocode(acoup.Rx, acoup.Ry)]==1)){
                //alert("Coup possible (type 2) "+ecrire_coups(myPlateau.Coups[c+1]));
                liste_coups.push(myPlateau.Coups[c+1]);
            };};};
    return liste_coups;
}

/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/

function derivation (mypath, pos, myhash){
    this.chemin = mypath; // une liste de coups
    this.position_atteinte = pos; 
    this.hashcode = myhash; // hashcode de la position atteinte
}

function chemins_possibles(pos_ini, longueur, myPlateau){
    var chemins = [[]], newchemins, cp, newpos, newone;

    for(var i = 1; i<= longueur; i++){
        newchemins = new Array;
        for(var n = 0; n < chemins.length; n++){
            //alert(ecrire_chemin(chemins[n]));
            cp = coups_possibles(pos_ini, chemins[n], myPlateau);
            //alert(ecrire_chemin(chemins[n]));
            for(var c = 0; c < cp.length; c++){
                if(chemins[n].length==0){
                    newone = new Array (cp[c]);
                    //alert("B: "+ecrire_chemin(newone));
                    newchemins.push(newone);
                    //alert("B: "+ecrire_chemin(newone)+" :: "+newchemins.length);
                } else {
                    //alert(ecrire_chemin(chemins[n])+"  "+(typeof chemins[n]));
                    newone = chemins[n].slice(0);
                    newone.push(cp[c]);
                    newchemins.push(newone);
                    // alert("A: "+ecrire_chemin(newone)+" :: "+newchemins.length);
                };};    
        };
        chemins = newchemins;
        if(chemins.length == 0){return [];};};
    return chemins;
}

function already_atteinte(myder, derivations, myPlateau){

    if(derivations.length == 0){return 0;};
 
    for(var nb = 0; nb < derivations.length; nb++){
        if(myder.hashcode == derivations[nb].hashcode){
            if(eq_pos(myder.position_atteinte, derivations[nb].position_atteinte)==1){
                //alert("Already: "+ecrire_chemin(myder.chemin));
                //alert(myder.position_atteinte+"\n"+derivations[nb].position_atteinte);
                return 1;
            }; //else {alert(myder.position_atteinte+" !=\n"+derivations[nb].position_atteinte);}
        }};
    return 0;
}

function derivations_possibles(pos_ini, longueur, myPlateau){
    var chemins, derivations = [], myder, new_pos, nbkilledbyhash = 0;
    
    chemins = chemins_possibles(pos_ini, longueur, myPlateau);
    for(var c = 0; c < chemins.length; c++){
        new_pos = joue_chemin(pos_ini, chemins[c], myPlateau);
        myder = new derivation(chemins[c], new_pos, hashcode(new_pos, myPlateau));
        
        if(already_atteinte(myder, derivations, myPlateau)==0){
            if(derivations.length == 0){
                derivations = new Array(myder);
           } else {
                derivations.push(myder);
           };} else {nbkilledbyhash++;};
    };
    //alert(nbkilledbyhash + "derivations killed by hashtable");
    return derivations;
}

function derivations_possibles_plus(myJeu, longueur, myPlateau){
    var qui, chemins, derivations = [], myder, new_pos, pos_ini, nbkilledbyhash = 0;
    
    for(qui = 0; qui < myJeu.derivations.length; qui++){   
        pos_ini = myJeu.derivations[qui].position_atteinte;
        chemins = chemins_possibles(pos_ini, longueur, myPlateau);
        for(var c = 0; c < chemins.length; c++){
            new_pos = joue_chemin(pos_ini, chemins[c], myPlateau);
            myder = new derivation(myJeu.derivations[qui].chemin.concat(chemins[c]), 
                                   new_pos, hashcode(new_pos, myPlateau));
            
            if(already_atteinte(myder, derivations, myPlateau)==0){
                if(derivations.length == 0){
                    derivations = new Array(myder);
                } else {
                    derivations.push(myder);
                };} else { nbkilledbyhash++;};
        };}
    //alert("[plus] "+nbkilledbyhash + "derivations killed by hashtable");
    return derivations;
}

function prepare_ground(){
    no_display_derivations();
    document.getElementById("compared_carac").innerHTML = ""; 
    document.getElementById("compared_pot_elem").innerHTML = ""; 
    document.getElementById("display_derivations").innerHTML = "";
    document.getElementById("derivations").innerHTML = "";
    document.getElementById("derived").innerHTML = "";
    document.getElementById("derivedviajava").innerHTML = "";
    document.getElementById("play_derivation").innerHTML = ""; 
    document.getElementById("restore_ini").innerHTML = "";
    document.getElementById("show_path").innerHTML = "";
    Etude_Graphique_display_position_initiale(); 
}

function essaie_de_deriver_inner(myJeu, myPlateau, nbdone, nbsteps){
    if((nbsteps > nbdone)&&(myJeu.derivations.length > 0)){
        myJeu.derivations = derivations_possibles_plus(myJeu, 
                                                       Math.min(longueur_pas_descente, nbsteps-nbdone), myPlateau);
        nbdone += Math.min(longueur_pas_descente, nbsteps);
        document.getElementById("derived").innerHTML += 
        "<br/>" + message[15][language] + myJeu.derivations.length
            + message[16][language] + nbdone + message[17][language];
        nettoyage_par_pot_elem(myJeu, myPlateau);
        document.getElementById("derived").innerHTML += 
        "<br/><img src=\"../../trucs/arrowhead-right.jpg\">&nbsp; " +
            message[18][language] + myJeu.derivations.length
            + message[19][language];
        //nbsteps += -Math.min(longueur_pas_descente, nbsteps); 
        setTimeout(function(){
                       return essaie_de_deriver_inner(myJeu, myPlateau, nbdone, nbsteps);}, 2);
    } else {
        essaie_de_deriver_conclude(myJeu, myPlateau, nbdone, nbsteps);
    }
}   

function essaie_de_deriver_conclude(myJeu, myPlateau, nbdone, nbsteps){
    var done = 0;
    
    if(myJeu.derivations.length == 0){
        done = 0;
    } else {//alert(myJeu.derivations.length);
        for(var d = 0; d < myJeu.derivations.length; d++){
            if(eq_pos(myJeu.derivations[d].position_atteinte, myJeu.position_finale) == 0){
                myJeu.derivations.splice(d, 1);
                d--};};//alert(myJeu.derivations.length);
        if(myJeu.derivations.length == 0){done = 0;} else {done = 1;};
    };

     if(done == 0){
        document.getElementById("derived").innerHTML += 
        "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style=\"color:#A00000\">"
         +message[20][language] + "</span>";
    } else  {
        if(myJeu.derivations.length >1){
            /* One should never be here !!!*/
            display_derivations(myJeu, myPlateau);};
        document.getElementById("derived").innerHTML += 
        "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style=\"color:#FF00FF\">"
            + message[21][language] + "</span>";
        document.getElementById("play_derivation").innerHTML += 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
            + " onClick=\"play_derivation()\" value=\"PlayIt!\">";
        document.getElementById("restore_ini").innerHTML += 
        "<input type=\"button\" style=\"width:200px;background-color:#FFFFCC\""
            + " onClick=\"restore_ini()\" value=\"Restore position initiale\">";
        document.getElementById("show_path").innerHTML += 
        "<input type=\"button\" style=\"width:200px;background-color:#FFFFCC\""
            + " onClick=\"alert(ecrire_chemin(Etude.derivations[0].chemin))\" value=\""
            + message[22][language] + "\">";
    };
}   

function essaie_de_deriver(myJeu, myPlateau){
    var nbsteps, allres, res = 0, nbdone = 0;

    prepare_ground();
    /*--------------  Verif caracteristique modulo 2 : -----------*/
    nbsteps = nbpions(myJeu.position_initiale, myPlateau) - nbpions(myJeu.position_finale, myPlateau);
    allres = compare_caracteristiques(myJeu, myPlateau);
    for(var c = 0; c < allres.length; c++){ res += allres[c];};
    if(nbsteps <= 0){
         document.getElementById("derived").innerHTML = message[23][language];
        return;
    } else if(res == 1){
        document.getElementById("derived").innerHTML = message[24][language];
        return;
    } else if(res != 0){
        document.getElementById("derived").innerHTML = res + message[25][language];
        return;
    };

    /*--------------  Let's go --------------------*/
    /*-------------- Avoid being a spoiler !! -----*/
    if(show_time_four != 19){
      nbsteps = Math.min(nbsteps, 10);
    }
    nbsteps = Math.min(nbsteps, max_descente);
    nbdone = Math.min(longueur_pas_descente, nbsteps);
    myJeu.derivations = derivations_possibles(myJeu.position_initiale, nbdone, myPlateau);
    document.getElementById("derived").innerHTML = 
        message[15][language] + myJeu.derivations.length + message[16][language]
        + longueur_pas_descente + message[17][language];
    nettoyage_par_pot_elem(myJeu, myPlateau);
    document.getElementById("derived").innerHTML += 
    "<br/><img src=\"../../trucs/arrowhead-right.jpg\">&nbsp; "
        + message[18][language] + myJeu.derivations.length + message[19][language];
    
    //nbsteps += -nbdone;
    essaie_de_deriver_inner(myJeu, myPlateau, nbdone, nbsteps);
}

function essaie_de_deriver_via_java(myJeu, myPlateau){
    var nbsteps, allres, res = 0, nbdone = 0;

    prepare_ground();
    /*--------------  Verif caracteristique modulo 2 : -----------*/
    nbsteps = nbpions(myJeu.position_initiale, myPlateau)-nbpions(myJeu.position_finale, myPlateau);
    allres = compare_caracteristiques(myJeu, myPlateau);
    for(var c = 0; c < allres.length; c++){ res += allres[c];};
    /*-------------- Avoid being a spoiler !! -----*/
    if(nbsteps <= 0){
         document.getElementById("derived").innerHTML = message[23][language];
        return;
    } else if(res == 1){
        document.getElementById("derived").innerHTML = message[24][language];
        return;
    } else if(res != 0){
        document.getElementById("derived").innerHTML = 
            res + message[25][language];
        return;
    };
    if((show_time_four != 19)&&(nbsteps>10)){
        alert(message[45][language]);
        return;
    }

    /* document.JavaLibrary.initPlateauEtude();
     * has been used in Plateau_Graphique_set_plateau() from SolitaireEtude.js
     */
    try{
        document.JavaLibrary.JavaGoesToWork();
    } catch (ite) {
        alert("Automate throws: " + ite.toString())
    }
    
}

/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/

function score_pot(pos_ini, pos_fin, pot, myPlateau){
    var x, y, score = 0, code;
    for(x = 0; x < myPlateau.hauteur; x++){
        for(y = 0; y < myPlateau.largeur; y++){
            code = myPlateau.xytocode(x, y);
            if(myPlateau.Table[code]!=9){
                score += (pos_ini[code]-pos_fin[code])*pot[code];}}};
    return score;
}

function nbpions(pos, myPlateau){
    var x, y, nb = 0, code;
    for(x = 0; x < myPlateau.hauteur; x++){
        for(y = 0; y < myPlateau.largeur; y++){
            code = myPlateau.xytocode(x, y);
            if(myPlateau.Table[code]!=9){nb += pos[code];}}};
    return nb;
}

function nettoyage_par_pot_elem(myJeu, myPlateau){
    var qui, c;

    for(qui = 0; qui < myJeu.derivations.length; qui++){
        for(c = 0; c< myPlateau.liste_pot_elem.length; c++){
            if(score_pot(myJeu.derivations[qui].position_atteinte, myJeu.position_finale, 
                         myPlateau.liste_pot_elem[c], myPlateau) < 0) {
                myJeu.derivations.splice(qui, 1);
                c = myPlateau.liste_pot_elem.length;
                qui--;}}};
}

/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/

function display_derivations(myJeu, myPlateau) {
    var c, x, y, tt, newHTML="", nb = 0;
    
    newHTML += "<table><tr>";
    for(c=0; c< myJeu.derivations.length; c++){
        newHTML += " <td><table>";
        for(x=0; x< myPlateau.hauteur; x++){
         newHTML += "<tr>";
           for(y=0; y< myPlateau.largeur; y++){
                tt = myJeu.derivations[c].position_atteinte[myPlateau.xytocode(x,y)];
                if(tt == 9){
                    newHTML += "<td> </td>";
                } else if (tt == 1){
                    newHTML += "<td style=\"width:10px;background-color:#FFFF66;text-align:center\">1</td>";
                } else if (tt == 0){
                    newHTML += "<td style=\"width:10px;background-color:#C0C0C0;text-align:center\">0</td>";
                } else {
                    newHTML += "<td style=\"width:10px;background-color:red;text-align:center\">"+tt+"</td>";
                    ;};};
            newHTML += "</tr>";
        };
        nb++;
        if(nb>=nb_derivations_displayed_per_line*max_nb_lines_for_derivations){
            alert("Il y a trop de derivations, je refuse de les afficher toutes !!");
            c = myJeu.derivations.length;
            newHTML += "</table></td><td>...........</td>";
        } else {
            newHTML += "</table></td><td>&nbsp;&nbsp;&nbsp;</td>";
        }
        if((nb % nb_derivations_displayed_per_line) == 0){
            newHTML += "</tr><tr>";};
    };
    newHTML += "</tr></table>";
    document.getElementById("derivations").innerHTML = newHTML;
    document.getElementById("display_derivations").innerHTML = 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
        +" onClick=\"window.no_display_derivations()\" value=\"Cacher les derivations\">";
};

function no_display_derivations() {
    
    document.getElementById("derivations").innerHTML = "";
    document.getElementById("display_derivations").innerHTML = 
        "<input type=\"button\" style=\"width:250px;background-color:#FFFFCC\""
        +" onClick=\"window.display_derivations(Etude, PlateauEtude)\""
        +" value=\"Montrer les derivations\">";
};


function play_derivation_inner_one (chemin, qui, myPlateau){
    if(qui < chemin.length){
        var acoup = chemin[qui];
        setTimeout(function(){
                       Jeu_Graphique_mycoloring(acoup.Px, acoup.Py, explodecolor, myPlateau);
                       Jeu_Graphique_mycoloring(acoup.Qx, acoup.Qy, intermediatecolor, myPlateau);
                       Jeu_Graphique_mycoloring(acoup.Rx, acoup.Ry, selectedcolor, myPlateau);
                       return play_derivation_inner_two (chemin, qui, myPlateau);
                   } , show_time_one*50);
    };
}

function play_derivation_inner_two (chemin, qui, myPlateau){
    if(qui < chemin.length){
        var acoup = chemin[qui];
        setTimeout(function(){
                       Jeu_Graphique_myvalue(acoup.Px, acoup.Py, valuenix, myPlateau);
                       Jeu_Graphique_myvalue(acoup.Rx, acoup.Ry, valuepoint, myPlateau);
                       return play_derivation_inner_three (chemin, qui, myPlateau);
                   } , show_time_two*50);
    };
}

function play_derivation_inner_three (chemin, qui, myPlateau){
    var acoup = chemin[qui];
    setTimeout(function(){
                   Jeu_Graphique_mycoloring(acoup.Px, acoup.Py, initialcolor, myPlateau);
                   Jeu_Graphique_mycoloring(acoup.Qx, acoup.Qy, initialcolor, myPlateau);
                   Jeu_Graphique_myvalue(acoup.Qx, acoup.Qy, valuenix, myPlateau);
                   Jeu_Graphique_mycoloring(acoup.Rx, acoup.Ry, initialcolor, myPlateau);
                   return play_derivation_inner_one (chemin, qui+1, myPlateau);
               } , show_time_three*50);
}

function play_derivation (){
    restore_ini();
    play_derivation_inner_one (Etude.derivations[0].chemin, 0, PlateauEtude);
}



/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------*/

