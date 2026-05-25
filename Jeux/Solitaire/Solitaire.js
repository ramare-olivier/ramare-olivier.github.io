var Jeu_Graphique_message="";
var Nb_displayed_history_lines=0;
var initialcolor="#2EFEF7";
var selectedcolor="#F3F781";
var intermediatecolor="#C8FE2E";
var explodecolor="#58FA58";
var valuepoint = "  o  ";
var valuenix = "      ";
var lettres = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"];
<!-- Case : 0: case existante, aucun pion,
     //       1: case existante, un pion
     //       9: case inexistante
     //       -1: case existante, on ne sait pas si il y a un pion -->
var Anglais_pos1 = [9,9,1,1,1,9,9,
                    9,9,1,1,1,9,9,
                    1,1,1,1,1,1,1,
                    1,1,1,0,1,1,1,
                    1,1,1,1,1,1,1,
                    9,9,1,1,1,9,9,
                    9,9,1,1,1,9,9];

var Anglais_pos = [9,9,1,1,1,9,9,
                   9,9,1,1,1,9,9,
                   1,1,1,1,1,1,1,
                   1,1,1,0,1,1,1,
                   1,1,1,1,1,1,1,
                   9,9,1,1,1,9,9,
                   9,9,1,1,1,9,9];

<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->

function includeJS(jsPath){
  var js = document.createElement("script");
  js.setAttribute("type", "text/javascript");
  js.setAttribute("src", jsPath);
  document.getElementsByTagName("head")[0].appendChild(js);
}

function pausecomp (ms) {
    ms += new Date().getTime();
    while (new Date() < ms){}
}

function coup(myPx, myPy, myQx, myQy, myRx, myRy) {
    /* P + Q -> R */
    this.Px = myPx; /* vertical 0<= myPx < Plateau.hauteur */
    this.Py = myPy; /* horizontal   0<= myPy < Plateau.largeur */
    this.Qx = myQx;
    this.Qy = myQy;
    this.Rx = myRx;
    this.Ry = myRy;
}

function PlateauSolitaire(myhauteur, mylargeur, myTable, myname) {
    /* Une position de solitaire va juste etre un vecteur sur
     * une table carree qui a une hauteur et une largeur */
    this.name = myname;
    this.hauteur = myhauteur;
    this.largeur = mylargeur;
    /* Dans myPlateau, on met 9 pour dire que la case est absente, 
     * n'importe quelle autre valeur pour une case existante */
    this.Table =  myTable;
    this.nbcases =  0; // pas encore calcule
    this.Coups = new Array(); // pas encore calcule. C'est une liste de "coup".
                              // un coup et son inverse son cote a cote.
    this.liste_carac = []; // pas encore calcule.
    this.base_hash = [];  // Doit contenir 8 entiers
    this.liste_pot_elem = []; // pas encore calcule.
    // Attention !! Les potentiels n'ont pas un 9 aux cases manquantes.
}

/* The getters are for Java: */
PlateauSolitaire.prototype.getTable = function (c) {
    return(this.Table[c]);}

PlateauSolitaire.prototype.getbase_hash = function (c) {
    return(this.base_hash[c]);}

PlateauSolitaire.prototype.getlengthliste_carac = function () {
    return(this.liste_carac.length);}

PlateauSolitaire.prototype.getliste_carac = function (a,c) {
    return(this.liste_carac[a][c]);}

PlateauSolitaire.prototype.getlengthliste_pot_elem = function (a,c) {
    return(this.liste_pot_elem.length);}

PlateauSolitaire.prototype.getliste_pot_elem = function (a,c) {
    return(this.liste_pot_elem[a][c]);}

PlateauSolitaire.prototype.xytocode = function (x, y) {
    /* 0 <= x < myhauteur, 0 <= y < mylargeur */
    return(y + x * this.largeur);};

function Calculenbcases(myPlateau){
    var x, y, nb = 0;

    for( x = 0; x < myPlateau.hauteur; x++){
        for( y = 0; y < myPlateau.largeur; y++){
            if( myPlateau.Table[ myPlateau.xytocode( x, y)] != 9){ nb++;}}};
    return nb;
}

function CalculeCoups(myPlateau){
    var theCoups = new Array();
    var x, y, acoup;
    
    /* Coups horizontaux : */
    for( x = 0; x < myPlateau.hauteur; x++){
        for( y = 0; y < myPlateau.largeur-2; y++){
            if((myPlateau.Table[myPlateau.xytocode(x,y)] != 9)
                &(myPlateau.Table[myPlateau.xytocode(x,y+1)] != 9)
               &(myPlateau.Table[myPlateau.xytocode(x,y+2)] != 9)){
                acoup = new coup(x, y, x, y+1, x, y+2);
                theCoups.push(acoup);
                acoup = new coup(x, y+2, x, y+1, x, y);
                theCoups.push(acoup);
            };};};

    /* Coups verticaux : */
    for( x = 0; x < myPlateau.hauteur-2; x++){
        for( y = 0; y < myPlateau.largeur; y++){
            if((myPlateau.Table[myPlateau.xytocode(x,y)] != 9)
                &(myPlateau.Table[myPlateau.xytocode(x+1,y)] != 9)
               &(myPlateau.Table[myPlateau.xytocode(x+2,y)] != 9)){
                acoup = new coup(x, y, x+1, y, x+2, y);
                theCoups.push(acoup);
                acoup = new coup(x+2, y, x+1, y, x, y);
                theCoups.push(acoup);
            };};};
    return(theCoups);
}

<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->


function mymax(x, y){if(x<y){return(y);} else{return(x);};}
function mymin(x, y){if(x<y){return(x);} else{return(y);};}

function Populated_array( h, val){
    var x, res = new Array(h);
    for( x = 0; x<h; x++){ res[x] = val;};
    return(res);
}

function newchoiceplateau(myPlateau) {
    var i, j;
    for(i = 0; i<myPlateau.hauteur; i++){
        for(j = 0; j<myPlateau.largeur; j++){
            Plateau_Graphique_mycoloring(i, j, initialcolor, myPlateau);
        }};
}

<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->

function Jeu_Graphique_display_plateau(){
    var x, y, newHTML = "";

    PlateauCourant.Coups = CalculeCoups(PlateauCourant);
    PlateauCourant.nbcases = Calculenbcases(PlateauCourant);

    for(x = 0; x < PlateauCourant.hauteur; x++){
        newHTML = newHTML + "<tr>";
        for(y = 0; y < PlateauCourant.largeur; y++){
            if(PlateauCourant.Table[PlateauCourant.xytocode(x,y)]!=9){
                newHTML = newHTML + "<td><input type=\"button\" id=\"Jeu_Graphique_button_"+lettres[x]+lettres[y]+"\" value=\"    \" onClick=\"window.Jeu_Graphique_move("+x+","+y+",PlateauCourant)\"></td>";
            } else {
                newHTML = newHTML + "<td></td>";
            };};
        newHTML = newHTML + "</tr>";
    };
    document.getElementById("jeu_position_initiale").innerHTML = newHTML;
    
    newHTML = "";
    for(x = 0; x < PlateauCourant.hauteur; x++){
        newHTML = newHTML + "<tr>";
        for(y = 0; y < PlateauCourant.largeur; y++){
            if(PlateauCourant.Table[PlateauCourant.xytocode(x,y)]!=9){
                newHTML = newHTML + "<td><input type=\"button\" id=\"Jeu_Graphique_final_button_"+lettres[x]+lettres[y]+"\" value=\"    \" ></td>";
            } else {
                newHTML = newHTML + "<td></td>";
            };};
        newHTML = newHTML + "</tr>";
    };
    document.getElementById("jeu_position_finale").innerHTML = newHTML;
}

function Jeu_Graphique_display(myPlateau, position) {
    var nb_element=1;
    
    //alert("Displaying initial "+ myPlateau.name);
    Jeu_Graphique_display_final(myPlateau, position, "", 0);

   if(Jeu_Graphique_message != ""){
     if(Nb_displayed_history_lines == 2){
       Jeu_Graphique_message = "\r" + Jeu_Graphique_message;
       Nb_displayed_history_lines = 0;};
     document.forms[myPlateau.name].Jeu_Graphiquehistorique.value
       = document.forms[myPlateau.name].Jeu_Graphiquehistorique.value + Jeu_Graphique_message + ", ";
     Nb_displayed_history_lines++;
   };
   document.forms[myPlateau.name].Jeu_Graphiquehistorique.scrollTop = 
        document.forms[myPlateau.name].Jeu_Graphiquehistorique.scrollHeight;
   Jeu_Graphique_win(myPlateau);
}

function Jeu_Graphique_win(myPlateau) {  
  var ok, i;
  ok = 1;
  for(i = 0; i < (myPlateau.hauteur)*(myPlateau.largeur) & ok==1; i++){
    if(Jeu.position_initiale[i] != Jeu.position_finale[i]){
      ok = 0;}};
  if(ok == 1){
      if (confirm('You did it! Do you want to restart?')) newgame();}
}

function Jeu_Graphique_display_final(myPlateau, position, suffix, docolor) {
    var x, y, bouton;

    for(x = 0; x < myPlateau.hauteur; x++){
        for(y = 0; y < myPlateau.largeur; y++){
            bouton = myPlateau.name+suffix+"_button_"+lettres[x]+lettres[y];
             if(position[myPlateau.xytocode(x, y)] == 1){
                document.getElementById(bouton).value = valuepoint;
                if(docolor==1){Jeu_Graphique_mycoloring_final(x, y, initialcolor, myPlateau, suffix);};
            } else if(position[myPlateau.xytocode(x, y)] == -1){
                document.getElementById(bouton).value ="  ?  ";
                if(docolor==1){Jeu_Graphique_mycoloring_final(x, y, initialcolor, myPlateau, suffix);};
            } else if (position[myPlateau.xytocode(x, y)] != 9){ 
           //alert(bouton+" "+position[myPlateau.xytocode(x, y)]);
                document.getElementById(bouton).value = valuenix;
                if(docolor==1){Jeu_Graphique_mycoloring_final(x, y, initialcolor, myPlateau, suffix);};
            } else {
            }
        }};
}

function Jeu_Graphique_set_problematique () {
    document.forms[PlateauCourant.name].Jeu_Graphiqueproblematique.value = Jeu.problematique_message;
    document.forms[PlateauCourant.name].Jeu_Graphiqueproblematique.style.backgroundColor = "#8080ff";
}

function Jeu_Graphique_mycoloring(x, y, mycolor, myPlateau){
    document.getElementById(myPlateau.name+"_button_"+lettres[x]+lettres[y]).style.background=mycolor;
    return;
}

function Jeu_Graphique_mycoloring_final(x, y, mycolor, myPlateau, suffix){
    document.getElementById(myPlateau.name+suffix+"_button_"+lettres[x]+lettres[y]).style.background=mycolor;
    return;
}

function middle(x1, x2){
  if(x1 == x2) return x1;
    if((x1+x2)%2 == 0){
        return((x1+x2)/2);
    }
    else {
        return(-1);
    }
}

function Jeu_Graphique_switch(x,y, myPlateau){
   var aux;
   aux = Jeu.position_initiale[x];
   Jeu.position_initiale[x] = Jeu.position_initiale[y];
   Jeu.position_initiale[y] = aux;
}

function ecrire_coup(acoup) {
    return lettres[acoup.Px]+lettres[acoup.Py]
        +"+"+lettres[acoup.Qx]+lettres[acoup.Qy]
        +"->"+lettres[acoup.Rx]+lettres[acoup.Ry];
}

function Jeu_Graphique_move(x, y, myPlateau) {
    var x3, y3, x2, y2, acoup;
    x2 = Jeu.selectedstartx;
    y2 = Jeu.selectedstarty;
    if((x2 == -1)& (Jeu.position_initiale[myPlateau.xytocode(x, y)] == 1)){
        Jeu.selectedstartx = x;
        Jeu.selectedstarty = y;
        Jeu_Graphique_message="";
        Jeu_Graphique_mycoloring(x, y, selectedcolor, myPlateau);
    } else if ((x2 == x & y2 == y)& (Jeu.position_initiale[myPlateau.xytocode(x, y)] == 1)) {
        Jeu_Graphique_message="";
        Jeu.selectedstartx = -1;
        Jeu_Graphique_mycoloring(x, y, initialcolor, myPlateau);
    } else {
        x3 = middle(x, x2);
        y3 = middle(y, y2);//alert(x3+y3+" "+Jeu.position_initiale[myPlateau.xytocode(x, y)]);
        if(x3 == -1 | y3 == -1 | Jeu.position_initiale[myPlateau.xytocode(x3, y3)] == 9
           | Jeu.position_initiale[myPlateau.xytocode(x, y)] == 1
           | Jeu.position_initiale[myPlateau.xytocode(x2, y2)] == 0){
            Jeu_Graphique_message = "Impossible!";
        } else {
            Jeu.position_initiale[myPlateau.xytocode(x, y)] = 1;
            Jeu.position_initiale[myPlateau.xytocode(x3, y3)] = 0;
            Jeu.position_initiale[myPlateau.xytocode(x2, y2)] = 0;
            Jeu.selectedstartx = -1;
            /*  Clignotement pour le changement : */
            Jeu_Graphique_mycoloring(x3, y3, explodecolor, myPlateau);
            Jeu_Graphique_mycoloring(x2, y2, intermediatecolor, myPlateau);
            //
            setTimeout(function(){
                           Jeu_Graphique_mycoloring(x, y, initialcolor, myPlateau);
                           Jeu_Graphique_mycoloring(x3, y3, initialcolor, myPlateau);
                           Jeu_Graphique_mycoloring(x2, y2, initialcolor, myPlateau);} , 400);
            acoup = new coup(x2, y2, x3, y3, x, y);//alert("Cling !! for "+ myPlateau.name);
            Jeu_Graphique_message = ecrire_coup(acoup);
            Jeu.historique.push(acoup);
        };
    };
    Jeu_Graphique_display(myPlateau, Jeu.position_initiale);
}

function Jeu_Graphique_backward() {
    var acoup;
    if(Jeu.historique.length == 0){
    } else {
        acoup = Jeu.historique.pop();
        Jeu.position_initiale[PlateauCourant.xytocode(acoup.Px, acoup.Py)] = 1;
        Jeu.position_initiale[PlateauCourant.xytocode(acoup.Qx, acoup.Qy)] = 1;
        Jeu.position_initiale[PlateauCourant.xytocode(acoup.Rx, acoup.Ry)] = 0;
        /*  Clignotement pour le changement : */
        Jeu_Graphique_mycoloring(acoup.Rx, acoup.Ry, explodecolor, PlateauCourant);
        Jeu_Graphique_mycoloring(acoup.Qx, acoup.Qy, intermediatecolor, PlateauCourant);
        Jeu_Graphique_mycoloring(acoup.Px, acoup.Py, intermediatecolor, PlateauCourant);
        setTimeout(function(){
                       Jeu_Graphique_mycoloring(acoup.Px, acoup.Py, initialcolor, PlateauCourant);
                       Jeu_Graphique_mycoloring(acoup.Qx, acoup.Qy, initialcolor, PlateauCourant);
                       Jeu_Graphique_mycoloring(acoup.Rx, acoup.Ry, initialcolor, PlateauCourant);} , 400);
        Jeu_Graphique_message = 
            lettres[acoup.Rx]+lettres[acoup.Ry]+"->"+lettres[acoup.Qx]+lettres[acoup.Qy]+"+"+lettres[acoup.Px]+lettres[acoup.Py];
        Jeu_Graphique_display(PlateauCourant, Jeu.position_initiale);
    };
}

function newgame() {
    var i, j, newpb;
    /* Message par defaut : */
    Jeu.problematique_message="Il faut finir avec un seul pion !";           
    switch (document.forms[PlateauCourant.name].Debut.value){
    case "d1": newpb = Anglais; break;
    case "d2": newpb = Diamant; break;
    case "d3": newpb = Arc_court; break;
    case "d4": newpb = Arc_long; break;
    case "d5": newpb = Verre; break;
    case "d6": newpb = Maison; break;
    case "d7": newpb = Arcades; break;
    case "d8": newpb = Yo; break;
    case "d9": newpb = Spirale; break;
    case "d10": newpb = Troupe; break;
    case "d11": newpb = Fenetre; break;
    case "d12": newpb = Deux_absents; break;
    case "d13": newpb = Bassin; break;
    case "d14": newpb = Salle; break;
    case "d15": newpb = Plus; break;
    case "d16": newpb = Triangle; break;
    case "d17": newpb = Jump; break;
    case "d18": newpb = Cheminee; break;
    case "d19": newpb = Calvaire; break;
    case "d20": newpb = Pyramide; break;
    case "d21": newpb = Double_croix; break;
    case "d22": newpb = Cristal; break;
    case "d23": newpb = Cours_carre; break;
    case "d24": newpb = Bassin; break;
    case "d25": newpb = Croix_de_Malte; break;
    case "d26": newpb = Equateur; break;
    case "d27": newpb = Carre_incline; break;
    case "d28": newpb = Calice; break;
    case "d29": newpb = Main_de_Fatma; break;
    case "d30": newpb = Wiegleb; break;
    case "d31": newpb = Cellulaire; break;
    case "d32": newpb = Flacon; break;
    case "d33": newpb = Deux_carres; break;
    case "d34": newpb = Big_e; break;
    case "d35": newpb = Petite_croix; break;
    case "d36": newpb = Big_r; break;
    case "d37": newpb = Big_t; break;
    case "d38": newpb = Big_x; break;
    default :  newpb = Anglais; break;
    };

    PlateauCourant.hauteur = newpb.hauteur;
    PlateauCourant.largeur = newpb.largeur;
    PlateauCourant.Table = newpb.ini;
    /* On cree une copie de newpb.ini parce qu'on va modifier cette copie en jouant : */
    Jeu.position_initiale = newpb.ini.slice(0);
    Jeu.position_finale = newpb.fin;
    Jeu.problematique_message = newpb.problematique_message;
    Jeu_Graphique_display_plateau();

  for( i = 0; i<PlateauCourant.hauteur; i++){
    for( j = 0; j<PlateauCourant.largeur; j++){
      if(Jeu.position_initiale[PlateauCourant.xytocode(i, j)]!=9){
      Jeu_Graphique_mycoloring(i, j, initialcolor, PlateauCourant);};};};

  Jeu.selectedstartx = -1;
  Jeu_Graphique_message = "";
  Nb_displayed_history_lines = 0;
  document.forms[PlateauCourant.name].Jeu_Graphiquehistorique.value = "";
  Jeu_Graphique_display_final(PlateauCourant, Jeu.position_finale, "_final", 1);
  Jeu_Graphique_set_problematique();
  Jeu_Graphique_display(PlateauCourant, Jeu.position_initiale);   
}

<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->
<!-- ----------------           MAIN            -------------------- -->
<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->

<!-- -------------------------------------- -->
includeJS("Problemes/Problemes.js");
<!-- -------------------------------------- -->


<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->
var PlateauCourant = new PlateauSolitaire(7, 7, Anglais_pos, "Jeu_Graphique");

var Jeu = {
    position_initiale : [],
    position_finale : [],
    selectedstartx : -1, // -1: aucune case de selectionnee 
    selectedstarty : -1, // -1: aucune case de selectionnee 
    historique : new Array(), //Un tableau de coups. 
    problematique_message : ""
};

Jeu.position_initiale = Anglais_pos;
Jeu.position_finale = [9,9,0,0,0,9,9,
                       9,9,0,0,0,9,9,
                       0,0,0,0,0,0,0,
                       0,0,0,1,0,0,0,
                       0,0,0,0,0,0,0,
                       9,9,0,0,0,9,9,
                       9,9,0,0,0,9,9];

PlateauCourant.Coups = CalculeCoups(PlateauCourant);
PlateauCourant.nbcases = Calculenbcases(PlateauCourant);

// alert("Le plateau a "+ PlateauCourant.Coups.length + " coups");

//    alert("Le plateau a "+ PlateauCourant.Table.length + " cases");
//    alert("Le plateau a "+ PlateauCourant.nbcases + " cases");
 //   alert("Le plateau a une hauteur de "+PlateauCourant.hauteur);
 //   alert("Le plateau a une hauteur de "+ PlateauCourant.Table[1]);
 //   alert(PlateauCourant.Table[1]);


<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->

