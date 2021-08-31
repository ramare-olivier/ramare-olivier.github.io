<SCRIPT LANGUAGE="php">
/////////////////////////////////////////////////////////////////////////////
////                     COMPTEUR DE TELECHARGEMENTS                     ////
////                      <15/10/99 Version 1.00>                        ////
////                      (c) <spineau@teaser.fr>                        ////
/////////////////////////////////////////////////////////////////////////////
////  ParamŠtre en entr‚e : $Fichier  : nom du fichier … t‚l‚charger (doit )
////                                    obligatoirement ˆtre dans le mˆme
////                                    r‚pertoire que le script.
////  Appel du script     : Voir doc
/////////////////////////////////////////////////////////////////////////////
///------Fonctions Lock et Unlock d'aprŠs Etienne De Toqueville--------------
Function lock($file) {
  $timeout =  30; // Timeout (secondes) PHP
  $retry   =   5; // Temps maxi (secondes) d'attente avant abandon
  $delay   = 0.1; // Durée d'attente (secondes) entre chaque test

  if (file_exists($file.".lck")) {
          $time = @filemtime($file.".lck");
          if ($time) {
                  $since = time() - $time;
                  if ($since > $timeout) unlink($file.".lck");
          }
  }

  for($i = 0; $i < $retry; $i += $delay) {
          if (!file_exists($file.".lck")) {
             $idlck=fopen($file.".lck","w");
             fclose($idlck);
             return 1;
          }
          usleep($delay * 1000000);
  }
  return 0;
}
///-------------------------------------------------------------------------
Function unlock($file) {
  $i = @unlink($file.".lck");
  return $i;
}
///-------------------------------------------------------------------------
Function Error($Msg){
  Echo $Msg;
  FinPres();
  Exit("");
}
////////////////////////////////////////////////////////////////////////////
Function Align($num){
////////////////////////////////////////////////////////////////////////////
   // Aligne un nombre sur 2 caractŠres avec des 0
   $str=$num;
   for ($x=strlen($str);$x<2;$x++){
       $str="0".$str;
   }
   return $str;
}
///-------------------------------------------------------------------------
Function PrintCompteur($File){
$FileCompteur="compteurs/".$File.".dat";
if (!file_exists($FileCompteur)){
   $Compteur=0;
}
else{
   if (!lock($FileCompteur)) Error("????");
   $Fp=fopen($FileCompteur,"r");
   $Compteur=fgets($Fp,10);
   fclose($Fp);
   unlock($FileCompteur);
}
Echo $Compteur;
}
</SCRIPT>
