<script language="php">

$FileCompteur="compteurs/".$Fichier.".dat";
///--- fonctions lock, unlock et Error de compteur.php3
///-------------------------------------------------------------------------
 include("compteur.php"); 

if (!file_exists($FileCompteur)){
   $Compteur = 1;
   $Fp = fopen($FileCompteur, "w");
   fputs($Fp, $Compteur);
   fclose($Fp);
}
else{
   if (!lock($FileCompteur)) Error("OccupÈ ! Recommencez plus tard. Merci.");
   $Fp = fopen($FileCompteur, "r");
   $Compteur = fgets($Fp, 10);
   fclose($Fp);
   $Compteur++;
   $Fp = fopen($FileCompteur, "w");
   fputs($Fp, $Compteur);
   fclose($Fp);
   unlock($FileCompteur);
}

header("Location: http://math.univ-lille1.fr/~ramare/Maths/".$Fichier); // redirection vers le t√√≈†l√√≈†chargement
exit;
</script>
