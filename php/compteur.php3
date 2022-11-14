<?php
  function get_compteur($fichier="php/compteurs/compteur-page-principale.txt")
  {
    $couleurtexte="#8D38C9";   
    $fp = @fopen($fichier, "r");  
    if (!$fp) {        
        echo "Impossible d'ouvrir $fichier en lecture";  
        exit;    } 
    $visites = fgets($fp, 8); 
    echo "<font color=$couleurtexte>"; 
    echo $visites++;  
    echo "</font>"; // on affiche $visites, et on increment $visites.
    fclose($fp); 
    $fp = @fopen($fichier, "w"); // le fichier est ouvert en ecriture, remis a zero 
    if (!$fp) {   
        echo "Impossible d'ouvrir $fichier en ecriture"; 
        exit;    }  
    fputs($fp, $visites);  
    fclose($fp);
  }   
?>