<script language="php">

$FileCompteur="compteurs/".$Fichier.".dat";
///--- fonctions lock, unlock et Error de compteur.php3
///-------------------------------------------------------------------------

if (!file_exists($FileCompteur)){
   $Compteur=1;
   $Fp=fopen($FileCompteur,"w");
   fputs($Fp,$Compteur);
   fclose($Fp);
}
else{
   if (!lock($FileCompteur)) Error("Occup� ! Recommencez plus tard. Merci.");
   $Fp=fopen($FileCompteur,"r");
   $Compteur=bcadd(trim(fgets($Fp,255)),"1",0);
   fclose($Fp);
   $Fp=fopen($FileCompteur,"w");
   fputs($Fp,$Compteur);
   fclose($Fp);
   unlock($FileCompteur);
}

header("Location: http://math.univ-lille1.fr/~ramare/Maths/".$Fichier); // redirection vers le tÃ©lÃ©chargement
exit;
</script>
