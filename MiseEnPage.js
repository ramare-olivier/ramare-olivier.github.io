var Architecture = [[["Home", "index.html"]], [["Publications", "Maths/contenu.html"], ["Excursions Typos", "Maths/ExcursionsInMultiplicativeNT-Typos/contenu-typos.html"]], [["Projet TME-EMT", "index.html"]], [["Jeux - Games", "Jeux/IndexJeux.html"], ["Solitaire", "Jeux/Solitaire/SolitaireJeu.html"], ["&Eacute;tude du Solitaire", "Jeux/Solitaire/Solitaire.html"]]];

var Architecture_en = [[["Home", "index.html"]], [["Publications", "Maths/contenu.html"], ["Excursions Typos", "Maths/ExcursionsInMultiplicativeNT-Typos/contenu-typos.html"]], [["The TME-EMT project", "https://tmeemt.github.io/Chest/"]], [["Games", "Jeux/IndexJeux.html"], ["Peg Solitaire", "Jeux/Solitaire/SolitaireJeu-en.html"], ["Study of Peg Solitaire", "Jeux/Solitaire/Solitaire-en.html"]]];

var Architecture_Cours = [["Le cours de Nouakchott 2012", "CoursNouakchott/index.html"],["Le cours de Monastir 2013", "CoursMonastir/index.html"],["Le cours de Nouakchott 2013", "CoursNouakchott2013/index.html"],["The Kolkata Lectures 2022", "CoursKolkata/index.html"]]; 

var Architecture_TME_EMT = [["Averages of arithmetical functions",[["Explicit bounds on primes","Articles/Art01.html"],["Explicit bounds on the Moebius function","Articles/Art02.html"],["Averages of non-negative multiplicative functions","Articles/Art10.html"],["Explicit pointwise upper bounds for some arithmetic functions","Articles/Art12.html"]]],["Exact computations",[["Exact computations of the number of primes","Articles/Art03.html"], ["Computations of arithmetical constants","Articles/Art04.html"]]],["General analytical tools", [["Tools on Fourier transforms","Articles/Art16.html"],["Tools on Mellin transforms","Articles/Art17.html"]]],["Exponential sums / points close to curves",[["Explicit results on exponential sums","Articles/Art05.html"],["Explicit results on points close to smooth plane curves","Articles/Art11.html"]]],["Size of $L(1,\\chi)$ and character sums",[["Size of $L(1,\\chi)$","Articles/Art07.html"],["Character sums", "Articles/Art15.html"]]],["Zeros and zero-free regions",[["Bounds for $|\\zeta(s)|$ and related questions","Articles/Art06.html"],["Explicit zero-free regions for the $\zeta$ and $L$ functions","Articles/Art08.html"]]],["Sieve and short interval results",[["Short intervals containing primes","Articles/Art09.html"],["Sieve bounds","Articles/Art14.html"]]],["Analytic Number Theory in Number Fields", [["Bounds on the Dedekind zeta-function", "Articles/Art18.html"]]], ["Applications",[["Explicit bounds for class numbers","Articles/Art13.html"], ["Primitive Roots", "Articles/Art19.html"]]],["Bibliography",[["Bibliography", "../"]]]];

function BandeauGeneral(numeroOn, Home, profondeur, lang){
    var newHTML = '<div id="bandeaugeneral" class="nav clearfix"> <ul>';
    /* Optional argument arg a valeur "fr" ou "en" */
    if (typeof lang == 'undefined') {
	lang = "fr";
    }
    var arch;
    if(lang == "fr"){
            arch = Architecture;
    } else {
        arch = Architecture_en;
    }
    /* The horizontal menu: */
    for(var x = 0; x < arch.length; x++){
        if(x == numeroOn){
            newHTML += '<li class="on">';
        } else {
            newHTML += '<li>';
        }
        if(x == 2){
	    newHTML += '<a href = "' + arch[x][0][1]
		+ '">'+ arch[x][0][0] + '</a></li>';
	} else {
            newHTML += '<a href = "' + Home + arch[x][0][1]
		+ '">'+ arch[x][0][0] + '</a></li>';}
    };
    newHTML += '</ul></div>';

    /* The arborescence: */
    newHTML += '<br /><p class="arbo">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;';
    
    if(numeroOn == 0){
        newHTML += '<strong class="on">' + arch[0][0][0]
            + '</strong></p>';
    } else if ((numeroOn == 1)){ 
        /*** Cours; in that case profondeur = a ***/
        newHTML += '<a href = "'  + Home + arch[0][0][1] 
            + '">' + arch[0][0][0]
            + '</a>  &gt; ';
        newHTML += '<a href = "'  + Home + arch[1][0][1] 
            + '#CoursNotes">' + arch[1][0][0]
            + '</a>  &gt; ';
        newHTML += '<strong class="on">' 
            + Architecture_Cours[profondeur][0]
            + '</strong></p>  ';
     } else if ((numeroOn == 2)&&(isNaN(profondeur))){ 
         /*** TME-EMT; in that case profondeur = [a,b] ***/
        newHTML += '<a href = "'  + Home  + arch[0][0][1] 
            + '">' + arch[0][0][0]
            + '</a>  &gt; ';
        newHTML += '<a href = "'  + Home + arch[2][0][1] 
            + '">' + arch[2][0][0]
            + '</a>  &gt; ';
        newHTML += '<strong class="on">' 
            + Architecture_TME_EMT[profondeur[0]][1][profondeur[1]][0]
            + '</strong></p>  ';
     } else if (numeroOn == 4){ 
        /*** Typos ***/
        newHTML += '<a href = "'  + Home + arch[0][0][1] 
            + '">' + arch[0][0][0]
            + '</a>  &gt; ';
        newHTML += '<a href = "'  + Home + arch[1][0][1] 
            + '">' + arch[1][0][0]
            + '</a>  &gt; ';
        newHTML += '<a href = "' 
             + Home + arch[1][0][1] + arch[1][1][1]
	     + '">'
	     +'<strong class="on">' + arch[1][1][0]
            + '</strong></a> &gt </p>  ';
   } else {
        newHTML += '<a href = "'  + Home + arch[0][0][1] 
            + '">' + arch[0][0][0]
            + '</a>  &gt; ';
        for(var y = 0; y < profondeur; y++){ /*** Jeux ***/
            newHTML += '<a href = "'  + Home + arch[numeroOn][y][1]
                + '">' + arch[numeroOn][y][0]
                + '</a> &gt; ';
        }
        newHTML += '<strong class="on">'  
            + arch[numeroOn][profondeur][0] + '</strong></p>';
    } 
    
    document.write( newHTML);
};

/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/

function DisplayPicture(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="trucs/Olivier-Ramare.jpg" width="150" >';
}

function DisplayPictureSurya(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><br/><br/><img align=baseline src="../trucs/Luminy2015-3.jpg" width="150" >';
}

function DisplayPicturePieter(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><br/><br/><img align=baseline src="../trucs/Pieter.jpeg" width="150" >';
}

function DisplayPictureAlisa(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><br/><br/><img align=baseline src="../trucs/alisa-sedunova-2019.jpg" width="150" >';
}

function DisplayPictureNouakchottOne(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="2012-12-06-0176.jpg" width="300" >';
}

function DisplayPictureNouakchottTwo(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="2012-12-06-10.jpg" width="250" >';
}

function DisplayPictureNouakchottThree(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="2012-12-06-11.jpg" width="250" >';
}

function DisplayPictureNouakchottFour(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="NouakchottNight.jpg" width="250" >';
}

function DisplayPictureMonastirOne(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="2013-05-21-2.JPG" width="350" >';
}

function DisplayPictureMonastirTwo(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="2013-05-22-3.JPG" width="350" >';
}

function DisplayPictureMonastirThree(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="2013-05-21-5.JPG" width="350" >';
}

function DisplayPictureMonastirFour(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="Monastir.jpg" width="250" >';
}

function DisplayPictureNouakchott2013One(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="Nouakchott-4-red.jpg" width="300" >';
}

function DisplayPictureNouakchott2013Two(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="24-10-2013-MonHotel-red.jpg" width="250" >';
}

function DisplayPictureNouakchott2013Three(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="29-10-2013-RedMorning-red.jpg" width="250" >';
}

function DisplayPictureNouakchott2013Four(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="PB020011-red.jpg" width="250" >';
}

function DisplayPictureNouakchott2013Five(){
    document.getElementById("TemporaryWindow").innerHTML =
        '<br/><br/><img align=baseline src="31-10-2013-Sunset-red.jpg" width="250" >';
}

function ClearTemporaryWindow(){
    document.getElementById("TemporaryWindow").innerHTML = "";
}

function DefaultGDTTANPicture(){
    document.getElementById("TemporaryWindow").innerHTML = 
        "<img src=\"../trucs/amphitheatre-d-athene.jpg\"  width=\"200\" >";
}

function TellDevTeam(){
    var newHTML ="";
    newHTML = "<div class=\"explanation\"><ul><li>Olivier Bordell&egrave;s</li><li>Pierre Dusart</li><li>Harald Helfgott</li><li>Pieter Moree</li><li>Akhilesh P</li><li>Olivier Ramar&eacute;</li><li>Enrique Trevi&ntilde;o</li></ul></div>";
    document.getElementById("TemporaryWindow").innerHTML = newHTML;
}

/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/

function Paper(Titre, AdditionalAuthor, hreflocal, journalwww, journal, volume, year, pagespan, hrefextern, comment) {
    var newHTML ="";
    
    newHTML += '<span class="titrearticle">' + Titre + '</span>';
    newHTML += ' <span class="ou">(' + year + ')</span>';
    newHTML += '<br>';
    if(AdditionalAuthor){
	newHTML += 'avec ' + AdditionalAuthor + '<br>';
    };
    newHTML += '<i>';
    newHTML += '<span class="ou"><a href="' + journalwww + '">' + journal + '</a>&nbsp; ';
    if(volume){
	newHTML += volume + ', pages ' + pagespan + '</span></i>';
    } else {
	newHTML += pagespan + ' pages</span></i>';
    }
    newHTML += '&nbsp;&nbsp;&nbsp; <span class="totheright"> <span class="Abstract">&raquo; Abstract</span>';
    if(hreflocal){
	newHTML += ' &nbsp;<a href="';
	newHTML += hreflocal + '" class="verydiscreet">&raquo; Local pdf</a>';
    };
    if(hrefextern){
	newHTML +=  '&nbsp;&nbsp;&nbsp;<a class="verydiscreet" href="' + hrefextern + '">&raquo; Journal full text (pdf)</a>';
    };
    newHTML += '</span>';
    if(typeof comment !== 'undefined'){
	newHTML += '<br><span class="ou">' + comment + '</span>';
    };
    newHTML += '<span class="bidule">+</span>';
     document.write( newHTML);
}

function ToAppear(Titre, AdditionalAuthor, hreflocal, journalwww, journal,year, nbpages) {
    var newHTML ="";
    
    newHTML += '<span class="titrearticle">' + Titre + '</span>';
    newHTML += ' <span class="ou">(' + year + ')</span>';
    newHTML += '<br>';
    if(AdditionalAuthor){
	newHTML += 'avec ' + AdditionalAuthor + '<br>';
    };
    newHTML += '<i>';
    newHTML += '<span class="ou">To appear in <a href="' + journalwww + '">' + journal + '</a>,&nbsp; ';
    newHTML += nbpages + ' pages</span></i>';
    newHTML += '&nbsp;&nbsp;&nbsp; <span class="totheright"> <span class="Abstract">&raquo; Abstract</span>';
    if(hreflocal){
	newHTML += ' &nbsp;<a href="';
	newHTML += hreflocal + '" class="verydiscreet">&raquo; Local pdf</a>';
    };
    newHTML += '</span>';
    newHTML += '<span class="bidule">+</span>';
    document.write( newHTML);
}

function Preprint(Titre, AdditionalAuthor, hreflocal, year, nbpages, note) {
    var newHTML ="";
    
    newHTML += '<span class="titrearticle">' + Titre + '</span>';
    newHTML += ' <span class="ou">(' + year + ')</span>';
    newHTML += '<br>';
    if(AdditionalAuthor){
	newHTML += 'avec ' + AdditionalAuthor + '<br>';
    };
    newHTML += '<i>';
    newHTML += '<span class="ou"> ' + note + ' ' + nbpages + ' pages</span></i>';
    newHTML += '&nbsp;&nbsp;&nbsp; <span class="totheright"> <span class="Abstract">&raquo; Abstract</span>';
    if(hreflocal){
	newHTML += ' &nbsp;<a href="';
	newHTML += hreflocal + '" class="verydiscreet">&raquo; Local pdf</a>';
    };
    newHTML += '</span>';
    newHTML += '<span class="bidule">+</span>';
     document.write( newHTML);
}

function Software(Titre, AdditionalAuthor, hreflocal, year, nblines, note) {
    var newHTML ='<img src="../trucs/cat-gets-mouse.png" class="catimage">';
    
    newHTML += '<span class="titresoftware">' + Titre + '</span>';
    newHTML += ' <span class="ou">(' + year + ')</span>';
    newHTML += '<br>';
    if(AdditionalAuthor){
	newHTML += 'avec ' + AdditionalAuthor + '<br>';
    };
    newHTML += '<i>';
    newHTML += '<span class="ou"> ' + note + ' ' + nblines + ' lines</span></i>';
    newHTML += '&nbsp;&nbsp;&nbsp; <span class="totheright"> <span class="Abstract">&raquo; Abstract</span>';
    if(hreflocal){
	newHTML += ' &nbsp;<a href="';
	newHTML += hreflocal + '" class="verydiscreet">&raquo; Local file</a>';
    };
    newHTML += '</span>';
    newHTML += '<span class="bidule">+</span>';
     document.write( newHTML);
}

function Appendix(Titre, TitreLivre, AuteurLivre, hreflocal, year, nbpages, hreflivre) {
    var newHTML ="";
    
    newHTML += '<span class="titrearticle">' + Titre + '</span>';
    newHTML += ' <span class="ou">(' + year + ')</span>';
    newHTML += '<br>';
    newHTML += '<i>';
    newHTML += '<span class="ou"> A ' + nbpages + '-pages long Appendix in ';
    newHTML += '<a href="' + hreflivre + '">"' + TitreLivre + '"</a> by ' + AuteurLivre + '</span></i>';
    newHTML += '&nbsp;&nbsp;&nbsp; <span class="totheright"> <span class="Abstract">&raquo; Abstract</span>';
    newHTML += ' &nbsp;<a href="';
    newHTML += hreflocal + '" class="verydiscreet">&raquo; Local pdf</a>';
    newHTML += '</span>';
    newHTML += '<span class="bidule">+</span>';
     document.write( newHTML);
}

function These(Auteur, webpageperso, Titre, hreflocal, date) {
    var newHTML ="";
    
    newHTML = '<span class="auteurthese">' + Auteur + '</span>';
    if(webpageperso){
	newHTML = '<a href="' + webpageperso + '">' + newHTML + '</a>';
    }
    newHTML += ' <span class="ou">(' + date + ')</span>';
    newHTML += '<br>';
    newHTML += '<i>';
    newHTML += '<a href="' + hreflocal + '">'; 
    newHTML += '<span class="ou">' + Titre + '</span></a></i>';
    newHTML += '&nbsp;&nbsp;&nbsp; <span class="totheright-small"> <span class="Abstract">&raquo; Abstract</span>';
    newHTML += '</span>';
    newHTML += '<span class="bidule">+</span>';
     document.write( newHTML);
}

function Memoire(Auteur, webpageperso, Titre, hreflocal, date, typediplome) {
    var newHTML ="";
    
    newHTML = '<span class="auteurthese">' + Auteur + '</span>';
    if(webpageperso){
	newHTML = '<a href="' + webpageperso + '">' + newHTML + '</a>';
    }
    newHTML += ' <span class="ou">(' + typediplome + ' ' + date + ')</span>';
    newHTML += '<br>';
    newHTML += '<i>';
    newHTML += '<a href="' + hreflocal + '">'; 
    newHTML += '<span class="ou">' + Titre + '</span></a></i>';
    
     document.write( newHTML);
}





/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/

function TMEEMTMenuold (){
    var newHTML = '<ul class="tme-emt-main-menu">';
    for(var x = 0; x < Architecture_TME_EMT.length-1; x++){
        newHTML +='<!-- '+ x + ' --> <li class="tme-emt-main-menu-item">' 
          /*--  +'&#9658;&nbsp;' --*/
            +'&rsaquo;&nbsp;'
            + Architecture_TME_EMT[x][0]
            + '<span class="tme-emt-second-menu"> <ul class="tme-emt-second-menu">';
        for(var y = 0; y < Architecture_TME_EMT[x][1].length; y++){
            newHTML +='<li class="tme-emt-second-menu-item">'
                + '<a href="' + Architecture_TME_EMT[x][1][y][1] + '">'
             /*--   + '&#8594;&nbsp;'  --*/
                + '&raquo;&nbsp;'
                + Architecture_TME_EMT[x][1][y][0]
                + '</a></li>';
            }
        newHTML += '</ul></span></li>';
    }
    newHTML += '<li class="tme-emt-main-biblio-item"><a href = "Biblio/Biblio.html">Bibliography</a></li></ul>';
    document.getElementById("TMEEMTMenu").innerHTML = newHTML;
}

function TMEEMTMenu (){
    var newHTML;
    for(var x = 0; x < Architecture_TME_EMT.length-1; x++){
        newHTML +='<!-- '+ x + ' --> <li class="tme-emt-main-menu-item-new">' 
          /*--  +'&#9658;&nbsp;' --*/
            +'&rsaquo;&nbsp;'
            + Architecture_TME_EMT[x][0]
            + '<span class="bidule">+</span>'
            + '<div class="resumecontent"><ul class="tme-emt-second-menu-new">';
        for(var y = 0; y < Architecture_TME_EMT[x][1].length; y++){
            newHTML +='<li class="tme-emt-second-menu-item-new">'
                + '<a href="' + Architecture_TME_EMT[x][1][y][1] + '">'
             /*--   + '&#8594;&nbsp;'  --*/
                + '&raquo;&nbsp;'
                + Architecture_TME_EMT[x][1][y][0]
                + '</a></li>';
            }
        newHTML += '</ul></div></li>';
    }
    document.getElementById("TMEEMTMenu").innerHTML = newHTML;
}


function shadeSelectedEntry (whoseName){
    var newStyle = document.createElement('style');
    
    /* Ne fonctionne pas, va falloir corriger !!*/
    newStyle.innerHTML = "#" + whoseName 
        + " {background-color: #0000FF;padding-top: 2px; padding-bottom: 2px;}";
    /* La partie qui suit fonctionne, ouf ! */
    newStyle.innerHTML = "a[name=\"" + whoseName +"\"]{";
    newStyle.innerHTML += "background-color: #C0C0C0; padding-top: 2px; padding-bottom: 2px;";
    newStyle.innerHTML += "padding-left: 3px; padding-right: 3px; font-size: larger;";
    newStyle.innerHTML += "}";

    document.body.appendChild(newStyle);
}


/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/
/*------ RefToCitation is defined in Biblio/RefToCitation.js ---------*/

function bibref(key) { 
    var newHTML = "[<a href='../Biblio/Biblio.html"; /*Il faut mettre l'ancre a la fin*/
    newHTML += "?SelectedEntry=\"" + key + "\"#" + key +"'>" + RefToCitation[key] 
        + "</a><span class=\"FullRef\">&nbsp;&dagger;<span class=\"TheRef\">"
        + "<span class=\"AuthorYear\">" + RefToAuthorYear[key] + "</span><br>" 
        + RefToTitle[key] + "<br>" + RefToRefs[key]
        + "</span></span>]";
    document.write(newHTML);}


/*--------------------------------------------------------------------*/
/*--------------------------------------------------------------------*/


// Add the sticky class to the navbar when you reach its scroll position. Remove "sticky" when you leave the scroll position


