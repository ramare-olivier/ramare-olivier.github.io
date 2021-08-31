;; File gmail.el  -*- emacs-lisp-mode -*-
;; 11 Janvier 2001
;; Maintainer: Olivier Ramare <ramare@NONONONOmath.univ-lille1.fr>



;; Utilisation :
;;
;; (1) Disons que l'on dispose d'une liste d'adresses email, les adresses etant separees par
(defconst gmail-separator "\n ,;\t")
;;     Cette liste est dans le buffer de nom buffer-to-sort.
;; (2) Alors, on execute
;;     (gmail-sort-list-of-eaddress buffer-to-sort)
;;     et l'on recupere dans le buffer "*Sorted*"
;;     une liste triee par nom de domaine, une adresse par ligne
;;     et chaque ligne contenant une adresse terminee par un ",".
;; (3) La il vaut mieux un oeil humain pour regarder cette liste
;;     et y detecter eventuellement des doublons non evidents.
;; (4) Ensuite on sauve cette liste dans un fichier, disons
;;     "Mailing-list".
;; (5) Enfin la commande
;;     (gmail-send "Nombres" "PremiereAnnonce.mail" "Mailing-list")
;;     envoie le fichier "PremiereAnnonce.mail" a toutes les
;;     adresses de "Mailing-list" avec comme sujet "Nombres",
;;     ou n'importe quelle chaine de caracteres qui vous convienne.
;;     Un mail a la fois, donc cela prend du temps. C'est mailx
;;     qui est invoque.

;; Le plus simple consiste donc a charger votre fichier d'adresses
;; sous emacs, puis dans un autre buffer charger ce fichier
;; (emacs doit alors passer en emacs-lisp-mode et un item "Emacs-Lisp"
;; doit appaitre dans votre barre de menu). Aller a la fin de ce fichier
;; et taper la commande a executer, puis utiliser l'item
;; "Emacs-Lisp/Byte-compile And Load", ce qui vous affiche le
;; buffer "*Sorted*". Regarder si ce fichier vous convient et sauver-le
;; la ou vous voulez, disons "Mailing-list". Ecriver votre message et
;; stockez-le dans un fichier, disons "MonMessage". Revenez au buffer
;; contenant ce programme, eliminer la ligne que vous avez entree
;; (c'etait (gmail-sort-list-of-eaddress "MonFichierAdressesMelangees"))
;; et entrez la nouvelle commande
;; (typiquement (gmail-send "Titre" "MonMessage" "Mailing-list"))
;; et c'est fait... Il faut attendre un peu car chaque message est envoye
;; separement.

(provide 'gmail)

(defun gmail-sort-list-of-eaddress (&optional buffer-to-sort out-buffer)
  "Trie une liste d'adresses email contenue dans le buffer
   BUFFER-TO-SORT dont la valeur par defaut est \"Prems\".
   Les adresses sont triees d'apres leur suffixe, puis par
   ordre lexicographique. Le resultat est mis dans le buffer
   OUT-BUFFER dont la valeur par defaut est \"*Sorted*\"."
   (let (; Des constantes que l'on peut changer:
         (buffer-to-sort-default "Prems")
         (tmp-buffer (make-temp-name "sort"))
         (out-buffer-default "*Sorted*")
         (separator gmail-separator)
         ;;--------------------------------------
         new-model-regexp new-item-regexp item-list item-to-sort-list)
        (if (stringp buffer-to-sort) nil
            (setq buffer-to-sort buffer-to-sort-default))
        (if (stringp out-buffer) nil
            (setq out-buffer out-buffer-default))
        (setq new-model-regexp
           (concat "[" separator "]\\(\\([^" separator "@]+\\)@\\([^" separator "\\.]*\\.\\)*\\(\\([^" separator "\\.]*\\)\\.\\([^" separator "\\.]+\\)\\)\\)[" separator "]"))
        ;; 1 --> l'adresse complete
        ;; 2 --> le nom
        ;; 4 --> nom-de-domaine . pays
        ;; 5 --> nom-de-domaine NOT USED
        ;; 6 --> pays           NOT USED
        (get-buffer-create tmp-buffer) (set-buffer tmp-buffer) (erase-buffer)
        (get-buffer-create out-buffer) (set-buffer out-buffer) (erase-buffer)
        (set-buffer buffer-to-sort)
        (copy-to-buffer tmp-buffer (point-min) (point-max))
        (set-buffer tmp-buffer) (switch-to-buffer tmp-buffer)
        (goto-char (point-min)) (insert "\n")
        (goto-char (point-max)) (insert "\n")
        (goto-char (point-min))
        (while (re-search-forward new-model-regexp (point-max) t)
           (setq new-item-regexp
                   (concat "[" separator "]\\(\\([^" separator "@]*\\)@[^" separator "]*"
                           (match-string 4) "\\)[" separator "]")
                 item-to-sort-list (list (cons (match-string 1) (match-string 2))))
           (set-buffer tmp-buffer)
           (delete-region (match-beginning 1) (match-end 1))
           (goto-char (match-beginning 1))
           (while (re-search-forward new-item-regexp (point-max) t)
              (add-to-list 'item-to-sort-list (cons (match-string 1) (match-string 2)))
              (delete-region (match-beginning 1) (match-end 1))
              (goto-char (match-beginning 1))
              )
            (set-buffer out-buffer)
            ;;----------------- SORTING .... ------------------------------
            (setq item-list (gmail-prepare item-to-sort-list))
            ;;----------------- 
            (goto-char (point-max))
            (mapcar (lambda (str) (insert str ",\n")) (nreverse item-list))
            (insert "\n") ; une ligne vide entre les groupes.
            (set-buffer tmp-buffer)
            (goto-char (point-min)))
         (kill-buffer tmp-buffer)
         (switch-to-buffer out-buffer)
         (goto-char (point-min))
         ))

(defun gmail-is-similar (a b)
  (let ((taille 4) (suba (list a)) (subb (list b)) (where 0))
     (while (<= where (- (length a) taille))
        (add-to-list 'suba (substring a where (+ taille where)))
        (setq where (1+ where)))
     (setq where 0)
     (while (<= where (- (length b) taille))
        (add-to-list 'subb (substring b where (+ taille where)))
        (setq where (1+ where)))
  (- (+ (length suba) (length subb))
     (length (progn (mapcar (lambda (x) (add-to-list 'suba x)) subb) suba)))))

(defun gmail-prepare (a-list)
  (let ((where 1) who model (ll (length a-list)) aux
        (my-array (vconcat a-list)))
     (while (< where ll)
            (setq who (1+ where) model (cdr (aref my-array (1- where))))
            (while (<= who ll)
                   (if (= (gmail-is-similar (cdr (aref my-array (1- who))) model) 0)
                       (setq who (1+ who)) ; fully different
                      (setq aux (aref my-array (1- who)))
                      (aset my-array (1- who) (aref my-array where))
                      (aset my-array where aux)
                      (setq who (1+ ll))))
            (setq where (1+ where)))
     (mapcar 'car my-array)))

(defun gmail-send (subject mail-file addresses-file)
  " Envoie le fichier MAIL-FILE a toutes les adresses
stockees dans ADDRESSES-FILE, le sujet etant
SUBJECT. Les adresses sont une par ligne, commencent en debut de ligne,
se terminent par une virgule."
  (save-excursion
   (get-buffer-create "*Addresses*")
   (set-buffer "*Addresses*")
   (insert-file addresses-file)
   (goto-char (point-min))
   (while (not (eobp))
     (shell-command
        (concat "mailx -s \"" subject "\"  "
                 (buffer-substring-no-properties (point) (progn (end-of-line) (1- (point))))
                 " < " (expand-file-name mail-file)))
     (skip-chars-forward "\n,"))
   (kill-buffer "*Addresses*")))


;; (gmail-sort-list-of-eaddress "Adresses.Melangees")

;; (gmail-send "Nombres Premiers a Lille : Annonce" "PremiereAnnonce.mail" MailingListeInitiale")
