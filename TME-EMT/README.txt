Utilisation :
=============
-- Les references necessaires sont introduites dans un fichier du repertoire
   TME-EMT/Articles dont le nom verifie les conditions suivantes :
   -- suffixe .html
   -- ne commence pas par .
   -- n'est pas Template_Article.html
   et bien sur, n'est pas repertoire.

-- Ces references sont introduites sous la forme
<script type="text/javascript">bibref("Wedeniwski*02")</script>
   sur une seule ligne.
   La reference Wedeniwski*02 correspond a une cle du fichier bibtex
   TME-EMT/Biblio/Local-TME-EMT.bib

-- Le script perl TME-EMT/Biblio/UpDateBiblio.pl doit etre lance a la
   main depuis le repertoire TME-EMT/Biblio/
   pour regenerer les pages web. Ce script va inspecter toutes les pages
   article decrites a l'alinea 1 ci-dessus, en sort les references, ecrit un
   fichier latex qu'il compile, puis lance bibtex et recompile deux
   fois. C'est en fait le fichier .bbl qui sert a extraire les donnees.
   Il y a toutefois des difficultes et le fichier
   TME-EMT/Biblio/ExceptionalRefs.txt sert a gerer ces exceptions.

   Qui plus est, ce script cree aussi le fichier booklet.tex qui est
   lui aussi compile, avec pdflatex. Chaque page donne lieu a un
   chapitre, et il n'y a pas d'ordre. Le rendu est approximatif.

-- Ce fichier TME-EMT/Biblio/ExceptionalRefs.txt contient des entrees au
   format 
cle-de-referencement
nom-pour-le-citer
nom-annee-pour-la-bibliographie
annee
titre
refs-diverses
   avec une seule ligne par entree
========================================== Fin ============
Usage:
======
-- The required references are introduced in a file of the directory
   TME-EMT/Articles whose name verifies the following conditions:
   -- suffix is .html
   -- doesn't start by .
   -- is not Template_Article.html
   and of course, is not a directory.

-- The references are introduced in the form
<script type="text/javascript">bibref("Wedeniwski*02")</script>
   on only one line
   The key Wedeniwski*02 corresponds to the key of the bibtex file
   TME-EMT/Biblio/Local-TME-EMT.bib


-- The Perl script TME-EMT/Biblio/UpDateBiblio.pl has to be run from
   the TME-EMT/Biblio/ directory to regenerate
   the web pages from the directory TME-EMT/Biblio/. This script will
   inspect all the article pages decribed in
   the first paragraph, takes the references out, write a latex file that uses 
   authordate1-4.sty and authordate1.bst, compiles the latex file, then bibtex 
   this file and recompile it twice more. The .bbl file is the one we need to
   extract the data.
   There are some difficulties and they are handled by the file
   TME-EMT/Biblio/ExceptionalRefs.txt, see this file for its format.

   Moreover, this script also creates the file booklet.tex, which is
   then compiled with pdflatex. Each html page gives rise to a
   chapter, no order being introduced. The outcome is rather rough.

-- The file TME-EMT/Biblio/ExceptionalRefs.txt contains entries
   formatted as follows:
key-to-quote
name-to-quote
name-year-for-the-bibliography
year
title
other-refs
   with a single line per entry.
========================================== End ============



