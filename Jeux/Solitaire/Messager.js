/* Messager.js : un programme pour etudier le solitaire
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
var language = 0; /* francais         */
                  /* 1 pour l'anglais */ 

var message= [["Deverouiller d'abord le plateau !","UnLock the table first!"],
              ["Verouiller le plateau","Lock the table"],
              ["DeVerouiller le plateau","UnLock the table"],
              [" cases,"," squares,"],
              ["Le plateau d'etude a ","The study table has "],
              ["et ", "and "],                              /* 5 */
              ["caracteristiques.", "characteristics. "], 
              ["Tableau de bord", "Tuning panel"], 
              ["Comparer les caracteristiques", "Compare the characteristics"], 
              [" coups"," moves"],
              ["Deltas des potentiels elementaires","Deltas of the elementary potentials"], /* 10 */
              ["Deriver via Javascript","Derive via Javascript"],
              ["Tout reprendre!!","Start all over!!"],
              ["Vider !","Empty it!"],
              ["Remplir","Fill"],
              ["Il y a ","There are "],                              /* 15 */
              [" derivations apres "," derivations after "],
              [" pas. "," steps. "],
              ["Il en reste ","There remains "],
              [" apres nettoyage par potentiels elementaires. ",
               " of them after cleansing via elementary potentials"],
              ["Probleme Impossible !!","Impossible!!"],               /* 20 */
              ["J'ai trouve !!!!","Found it!!!!"],
              ["Montrer !","Show!"],
              ["Il y a moins (ou autant) de pions au depart qu'a l'arrivee !",
               "There are less (or as many) pawns at beginning than at end!"],
              ["Une caracteristique de la position de depart n'est pas egale a celle de la position d'arrivee !",
               "A characteristic of the initial position is not equal to the one of the ending position!"],
              [" caracteristiques de la position de depart ne sont pas egales a celles de la position d'arrivee !",
               " characteristics of the initial position are not equal to the ones of the ending position!"],    /* 25 */
              ["Deriver via Java","Derive via Java"],
              ["Cacher les points de hashage","Hide hash points"],
              ["Montrer les points de hashage","Show hash points"],
              ["Nouveau pas de descente = ","New descending step = "],
              ["Pas de descente ","Descending step "],/* 30 */
              ["Nombre de derivations par ligne ","How many derivations by line "],
              ["Profondeur maximale pour deriver","Maximal investigating depth"],
              ["Remonter de tant ","Going up by "],
              ["Remonter de tant ","Going up by "],
              ["Show time,   premier pas ","Show time, step one   "], /* 35 */
              ["Show time,  deuxieme pas ","Show time, step two   "],
              ["Show time, troisieme pas ","Show time, step three "],
              ["Il y a ","There are "],
              [" branches apres avoir remonte de "," branches, after having being up by "],
              [" pas."," steps"], /* 40 */
              ["Branche no ","Branch no "],
              [" :",":"],
              ["Sous probleme impossible !!","Impossible branch!!"],               /* 43 */
              ["Show time, quatrieme pas","Show time, step four "],
              ["Nombre de coups >= 10 !! Cette version est bridee !","Number of steps >= 10 !! This is a restricted version!"], /* 45 */
              ["Montrer les positions finales des branches","Show final position in branches"],
              ["",""],
              ["",""],
              ["",""],
              ["",""], /* 50 */
              ["",""],
              ["",""],
              ["",""],
              ["",""],
              ["",""], /* 55 */
              ["",""]
             ];