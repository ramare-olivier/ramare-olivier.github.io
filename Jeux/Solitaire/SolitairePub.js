var Jeu_Graphique_message="";
var Nb_displayed_history_lines=0;
var initialcolor="#2EFEF7";
var selectedcolor="#F3F781";
var intermediatecolor="#C8FE2E";
var explodecolor="#58FA58";
var crossedoutcolor = "#0404B4";
var lettres = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"];
var largeurmaximale = 16;
var hauteurmaximale = 16;
var verrouplateau = 1; /* Si 0, on ne peut plus modifier le plateau */
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

function coup(myPx, myPy, myQx, myQy, myRx, myRy) {
    /* P + Q -> R */
    this.Px = myPx; /* vertical 0<= myPx < Plateau.hauteur */
    this.Py = myPy; /* horizontal   0<= myPy < Plateau.largeur */
    this.Qx = myQx;
    this.Qy = myQy;
    this.Rx = myRx;
    this.Ry = myRy;
}

function PlateauSolitaire(myhauteur, mylargeur, myPlateau, myname) {
    /* Une position de solitaire va juste etre un vecteur sur
     * une table carree qui a une hauteur et une largeur */
    this.name = myname;
    this.hauteur = myhauteur;
    this.largeur = mylargeur;
    /* Dans myPlateau, on met 9 pour dire que la case est absente, 
     * n'importe quelle autre valeur pour une case existante */
    this.Plateau =  myPlateau;
    this.nbcases =  0; // pas encore calcule
    this.Coups = new Array(); // pas encore calcule. C'est une liste de "coup".
}

PlateauSolitaire.prototype.xytocode = function (x, y) {
        /* 0 <= x < myhauteur, 0 <= y < mylargeur */
        return(y + x * this.largeur);
    };

function Calculenbcases(myPlateau){
    var nb = 0;

    for( x = 0; x < myPlateau.hauteur; x++){
        for( y = 0; y < myPlateau.largeur; y++){
            if( myPlateau.Plateau[ myPlateau.xytocode( x, y)] != 9){ nb++;}}};

    return(nb);
}

function CalculeCoups(myPlateau){
    var theCoups = new Array();
    var acoup;
    
    /* Coups horizontaux : */
    for( x = 0; x < myPlateau.hauteur; x++){
        for( y = 0; y < myPlateau.largeur-2; y++){
            if((myPlateau.Plateau[myPlateau.xytocode(x,y)] != 9)
                &(myPlateau.Plateau[myPlateau.xytocode(x,y+1)] != 9)
               &(myPlateau.Plateau[myPlateau.xytocode(x,y+2)] != 9)){
                acoup = new coup(x, y, x, y+1, x, y+2);
                theCoups.push(acoup);
                acoup = new coup(x, y+2, x, y+1, x, y);
                theCoups.push(acoup);
            };};};

    /* Coups verticaux : */
    for( x = 0; x < myPlateau.hauteur-2; x++){
        for( y = 0; y < myPlateau.largeur; y++){
            if((myPlateau.Plateau[myPlateau.xytocode(x,y)] != 9)
                &(myPlateau.Plateau[myPlateau.xytocode(x+1,y)] != 9)
               &(myPlateau.Plateau[myPlateau.xytocode(x+2,y)] != 9)){
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
    var res = new Array(h);
    for( x = 0; x<h; x++){ res[x] = val;};
    return(res);
}

function Plateau_Graphique_mycoloring(x, y, mycolor, myPlateau){
    document.getElementById(myPlateau.name+"_button_"+lettres[x]+lettres[y]).style.background=mycolor;
    return;
}

function Plateau_Graphique_select(x, y, myPlateau) {
    if(verrouplateau == 1){ alert("Dévérouiller d'abord le plateau !"); return;};
    //alert(myPlateau.Plateau[myPlateau.xytocode(x, y)]);
    if (myPlateau.Plateau[myPlateau.xytocode(x, y)] == 0) {
        myPlateau.Plateau[myPlateau.xytocode(x, y)] = 9;
        Plateau_Graphique_mycoloring(x, y, crossedoutcolor, myPlateau);
    } else {
        myPlateau.Plateau[myPlateau.xytocode(x, y)] = 0;
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

function Plateau_Graphique_ote_colonne(){
    if(verrouplateau == 1){ alert("Deverouiller d'abord le plateau !"); return;};

    if(largeurmaximale > 1){
        for(x = 0; x < PlateauMaximal.hauteur; x++){
            PlateauMaximal.Plateau[PlateauMaximal.xytocode(x, largeurmaximale-1)] = 9;
            Plateau_Graphique_mycoloring(x, largeurmaximale-1, crossedoutcolor, PlateauMaximal);
        };
        largeurmaximale--;
    };
}

function Plateau_Graphique_ajoute_colonne(){
    if(verrouplateau == 1){ alert("DeVerouiller d'abord le plateau !"); return;};

    if(largeurmaximale < PlateauMaximal.largeur){
        for(x = 0; x < PlateauMaximal.hauteur; x++){
            PlateauMaximal.Plateau[PlateauMaximal.xytocode(x, largeurmaximale)] = 0;
            Plateau_Graphique_mycoloring(x, largeurmaximale, initialcolor, PlateauMaximal);
        };
        largeurmaximale++;
        hauteurmaximale = PlateauMaximal.hauteur;
    };
    // alert("Nouvelle largeur = "+largeurmaximale);
}

function Plateau_Graphique_ote_ligne(){
    if(verrouplateau == 1){ alert("DeVerouiller d'abord le plateau !"); return;};
    //alert("hauteurmaximale = "+hauteurmaximale);
    if(hauteurmaximale > 1){
        for(y = 0; y < PlateauMaximal.largeur; y++){
            PlateauMaximal.Plateau[PlateauMaximal.xytocode(hauteurmaximale-1, y)] = 9;
            Plateau_Graphique_mycoloring(hauteurmaximale-1, y, crossedoutcolor, PlateauMaximal);
        };
        hauteurmaximale--;
    };
}

function Plateau_Graphique_ajoute_ligne(){
    if(verrouplateau == 1){ alert("DeVerouiller d'abord le plateau !"); return;};

    if(hauteurmaximale < PlateauMaximal.hauteur){
        for(y = 0; y < PlateauMaximal.largeur; y++){
            PlateauMaximal.Plateau[PlateauMaximal.xytocode(hauteurmaximale, y)] = 0;
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
        document.getElementById("VerrouPlateau").value="Verouiller le plateau";
    } else {
        verrouplateau = 1;
        document.getElementById("VerrouPlateau").value="DeVerouiller le plateau";
    }
}

function Plateau_Graphique_set_plateau(){
    var table = new Array();
    var hauteurmax = 0;
    var largeurmax = 0;
    var hauteurmin = PlateauMaximal.hauteur;
    var largeurmin = PlateauMaximal.largeur;
    if(verrouplateau == 0){ Plateau_Graphique_toggle_verrou();};

    for(x = 0; x< PlateauMaximal.hauteur; x++){
        for(y = 0; y< PlateauMaximal.largeur; y++){
            if(PlateauMaximal.Plateau[PlateauMaximal.xytocode(x,y)] != 9){
                hauteurmax = mymax(hauteurmax, x+1);
                largeurmax = mymax(largeurmax, y+1);
                hauteurmin = mymin(hauteurmin, x);
                largeurmin = mymin(largeurmin, y);
            };};};
 
    PlateauEtude = new PlateauSolitaire(hauteurmax-hauteurmin, largeurmax-largeurmin, 
                                        table, "Etude_Graphique");
    for(x = 0; x< PlateauEtude.hauteur; x++){
        for(y = 0; y< PlateauEtude.largeur; y++){
            PlateauEtude.Plateau[PlateauEtude.xytocode(x,y)] 
                = PlateauMaximal.Plateau[PlateauMaximal.xytocode(x,y)];}};
 
    PlateauEtude.Coups = CalculeCoups(PlateauEtude);
    PlateauEtude.nbcases = Calculenbcases(PlateauEtude);
    alert("Le plateau d'etude a " + PlateauEtude.Coups.length + " coups.");
    Set_Etude_Graphique_ini();
}

function Set_Etude_Graphique_ini(){
    var newHTML = "";
    Etude.position_initiale = new Array(PlateauEtude.hauteur * PlateauEtude.largeur);
    for(x = 0; x < PlateauEtude.hauteur; x++){
        newHTML = newHTML + "<tr>";
        for(y = 0; y < PlateauEtude.largeur; y++){
            if(PlateauEtude.Plateau[PlateauEtude.xytocode(x,y)]!=9){
                newHTML = newHTML + "<td><input type=\"button\" name=\"Etude_Graphique_button_"+lettres[x]+lettres[y]+"\" value=\"    \" onClick=\"window.Etude_Graphique_select("+x+","+y+",PlateauEtude)\"></td>";
            }else {
                newHTML = newHTML + "<td></td>";
            };};
        newHTML = newHTML + "</tr>";
    };
    document.getElementById("etude_position_initiale").innerHTML = newHTML;
}

function Etude_Graphique_select (){
}
<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->
<!-- ---------------------------------------------------------------------------- -->

function Jeu_Graphique_display_plateau(){
    var newHTML = "";

    PlateauCourant.Coups = CalculeCoups(PlateauCourant);
    PlateauCourant.nbcases = Calculenbcases(PlateauCourant);

    for(x = 0; x < PlateauCourant.hauteur; x++){
        newHTML = newHTML + "<tr>";
        for(y = 0; y < PlateauCourant.largeur; y++){
            if(PlateauCourant.Plateau[PlateauCourant.xytocode(x,y)]!=9){
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
            if(PlateauCourant.Plateau[PlateauCourant.xytocode(x,y)]!=9){
                newHTML = newHTML + "<td><input type=\"button\" id=\"Jeu_Graphique_final_button_"+lettres[x]+lettres[y]+"\" value=\"    \" ></td>";
            } else {
                newHTML = newHTML + "<td></td>";
            };};
        newHTML = newHTML + "</tr>";
    };
    document.getElementById("jeu_position_finale").innerHTML = newHTML;
    
}

function Jeu_Graphique_display(myPlateau) {
    var nb_element=1;
    
  for (var i = 0; i<(myPlateau.hauteur)*(myPlateau.largeur); i++)  {
     if(Jeu.position_initiale[i] != 9){
       if(Jeu.position_initiale[i] == 1){
          document.forms[myPlateau.name].elements[nb_element].value = "  o  ";}
       else { document.forms[myPlateau.name].elements[nb_element].value = "      ";};
       nb_element++;}};
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

function Jeu_Graphique_display_final(myPlateau) {
    var i, j, nb_element;
    nb_element = myPlateau.nbcases+1;

    for (i = 0; i<(myPlateau.hauteur)*(myPlateau.largeur); i++)  {
        if(Jeu.position_finale[i] != 9){
            if(Jeu.position_finale[i] == 1){
                document.forms[myPlateau.name].elements[nb_element].value = "  o  ";
            } else if(Jeu.position_finale[i] == -1){
                document.forms[myPlateau.name].elements[nb_element].value = "  ?  ";}
            else { document.forms[myPlateau.name].elements[nb_element].value = "      ";};
            nb_element++;}};

    for(i = 0; i<myPlateau.hauteur; i++){
        for(j = 0; j<myPlateau.largeur; j++){
            if(Jeu.position_finale[myPlateau.xytocode(i, j)]!=9){
                Jeu_Graphique_mycoloring_final(i, j, initialcolor, myPlateau);};};};
    document.forms[myPlateau.name].Jeu_Graphiqueproblematique.value = Jeu.problematique_message;
    document.forms[myPlateau.name].Jeu_Graphiqueproblematique.style.backgroundColor = "#8080ff";
}

function Jeu_Graphique_mycoloring(x, y, mycolor, myPlateau){
    document.getElementById(myPlateau.name+"_button_"+lettres[x]+lettres[y]).style.background=mycolor;
    return;
}

function Jeu_Graphique_mycoloring_final(x, y, mycolor, myPlateau){
    document.getElementById(myPlateau.name+"_final_button_"+lettres[x]+lettres[y]).style.background=mycolor;
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
            setTimeout(function(){
                           Jeu_Graphique_mycoloring(x, y, initialcolor, myPlateau);
                           Jeu_Graphique_mycoloring(x3, y3, initialcolor, myPlateau);
                           Jeu_Graphique_mycoloring(x2, y2, initialcolor, myPlateau);} , 400);
            Jeu_Graphique_message = lettres[x2]+lettres[y2]+"+"+lettres[x3]+lettres[y3]+"->"+lettres[x]+lettres[y];
            acoup = new coup(x2, y2, x3, y3, x, y);
            Jeu.historique.push(acoup);
        };
    };
    Jeu_Graphique_display(myPlateau);
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
        Jeu_Graphique_message = lettres[acoup.Rx]+lettres[acoup.Ry]+"->"+lettres[acoup.Qx]+lettres[acoup.Qy]+"+"+lettres[acoup.Px]+lettres[acoup.Py];
        Jeu_Graphique_display(PlateauCourant);
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
  default :  newpb = Anglais; break;
    };

    PlateauCourant.hauteur = newpb.hauteur;
    PlateauCourant.largeur = newpb.largeur;
    PlateauCourant.Plateau = newpb.ini;
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
  Jeu_Graphique_display_final(PlateauCourant);
  Jeu_Graphique_display(PlateauCourant);   
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
<!-- -------------------------------------- -->
var PlateauMaximal = new PlateauSolitaire(hauteurmaximale, largeurmaximale, 
                                          Populated_array(hauteurmaximale*largeurmaximale, 0),
                                          "Plateau_Graphique");

<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->
var PlateauCourant = new PlateauSolitaire(7, 7, Anglais_pos, "Jeu_Graphique");
var PlateauEtude;
 
var Jeu = {
    position_initiale : [],
    position_finale : [],
    selectedstartx : -1, // -1: aucune case de selectionnee 
    selectedstarty : -1, // -1: aucune case de selectionnee 
    historique : new Array(), //Un tableau de coups. 
    problematique_message : ""
};

var Etude = {
    position_initiale : [],
    position_finale : [],
    selectedstartx : -1, // -1: aucune case de selectionnee 
    selectedstarty : -1, // -1: aucune case de selectionnee 
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

//    alert("Le plateau a "+ PlateauCourant.Plateau.length + " cases");
//    alert("Le plateau a "+ PlateauCourant.nbcases + " cases");
 //   alert("Le plateau a une hauteur de "+PlateauCourant.hauteur);
 //   alert("Le plateau a une hauteur de "+ PlateauCourant.Plateau[1]);
 //   alert(PlateauCourant.Plateau[1]);


<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->
<!-- --------------------------------------------------------------- -->

