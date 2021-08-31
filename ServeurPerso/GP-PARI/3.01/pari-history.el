;; pari-history.el --  part of pari.el GP/PARI editing support package.
;;
;; To be used with pari.el version 3.00 or higher
;; pari-history.el version 3.01

;; documentation functions
;; See pariemacs-3.01.txt  for more details.

;; Most of this code is stolen from the mupad-run.el by Francois Maltey.
 
(provide 'pari-history)

;; Of pari.el, it uses:
;; variables:
;;     gp-prompt-pattern
(require 'pari-messages)
;; Of pari-messages.el, it uses gp-messager.
(eval-when-compile
  (fset 'x-defined-colors nil)
  ;; for development:
  ;;(setq byte-compile-warnings (list 'free-args 'unresolved 'callargs 'redefine 'obsolete))
  ;; for users:
  (setq byte-compile-warnings (list 'unresolved 'redefine 'obsolete))
  )
(eval-and-compile
(unless (fboundp 'gp-messager)
  (defun gp-messager (no) (print "Feature pari-messages is absent ..." "*Messages*")))
)

;(defvar gp-process nil "Defined in pari.el")

(defcustom gp-run-history-max 100
  "Maximum number of stored commands in history ring."
  :type 'integer :group 'gp-shell)

(defun gp-set-arrow-behaviour (symbol val)
  "See `gp-arrow-behaviour'"
  (setq gp-arrow-behaviour val)
  (cond 
    ((string= val "Usual")
      (define-key gp-map [(control up)] 
        (function gp-previous-history))
      (define-key gp-map [(control down)] 
        (function gp-next-history))
      (define-key gp-map [(up)] (function previous-line))
      (define-key gp-map [(down)] (function next-line)))
    (t ; for bash-style
      (define-key gp-map [(up)] 
        (function gp-previous-history))
      (define-key gp-map [(down)] 
        (function gp-next-history))
      (define-key gp-map [(control up)] (function previous-line))
      (define-key gp-map [(control down)] (function next-line)))))

(defcustom gp-arrow-behaviour
  "Usual"
  "Selects the behaviour of the arrow up and down :
  the usual behaviour corresponds to up and down
  while C-up and C-down correspond to history.
  When in Bash-Style, this behaviour in exchanbed."
  :type '(choice (const "Usual") (const "Bash-Style"))
  :initialize 'custom-initialize-default
;do not use gp-set-arrow-behaviour
;initially since the map is not yet defined !
  :set 'gp-set-arrow-behaviour
  :group 'gp-shell)

;; Internal global variables:

(defvar gp-hist-commands nil 
  "Variable that holds the history ring in gp-shell-mode")

;;;------------------------------------------------------

;; structure des données doublement chainée avec un point d'accès 
;; intermédiaire, sous forme d'un tableau :
;; coordonnées générale
;;   0 = liste directe - 1 = liste inverse - 2 = particulier - 3 = longueur.
;; L'accès aux éléments de la liste est aussi un tableau dont la valeur des 
;;   coordonnées est 
;; 0 = valeur - 1 = terme suivant - 2 = terme précédent
;;
(defun head-tail-void ()
  (let ((res (make-vector 4 nil)))
    (aset res 3 0)
    res))

(defun add-head (a struct) 
  (let ((br (make-vector 3 nil)) (tete (aref struct 0)))
    ;; define element. Value:
    (aset br 0 a) 
    ;; link to previous list:
    (aset br 1 tete)
    (if tete (aset tete 2 br) (aset struct 1 br))
    ;; set current value:
    (aset struct 0 br)
    ;; length is 1 larger:
    (aset struct 3 (1+ (aref struct 3)))
    struct))

(defun add-tail (a struct) 
  (let ((br (make-vector 3 nil)) (queue (aref struct 1)))
    (aset br 0 a) 
    (aset br 2 queue)
    (if queue (aset queue 1 br) (aset struct 0 br))
    (aset struct 1 br)
    (aset struct 3 (1+ (aref struct 3)))
    struct))

(defun remove-head (struct) 
  (unless (aref struct 0) (error "structure vide"))
  (when (eq (aref struct 2) (aref struct 0)) (aset struct 2 'head))
  (aset struct 0 (aref (aref struct 0) 1))
  (if (aref struct 0) (aset (aref struct 0) 2 nil) (aset struct 1 nil))
  (aset struct 3 (1- (aref struct 3)))
  struct)

(defun remove-tail (struct) 
  (unless (aref struct 0) (error "structure vide"))
  (when (eq (aref struct 2) (aref struct 1)) (aset struct 2 'tail))
  (aset struct 0 (aref (aref struct 0) 1))
  (if (aref struct 0) (aset (aref struct 0) 2 nil) (aset struct 1 nil))
  (aset struct 3 (1- (aref struct 3)))
  struct)

(defun list-tail (A) 
  (let ((tmp (aref A 0)) res)
    (while tmp (setq res (cons (aref tmp 0) res)) (setq tmp (aref tmp 1)))
    res))

(defun list-head (A) 
  (let ((tmp (aref A 1)) res)
    (while tmp (setq res (cons (aref tmp 0) res)) (setq tmp (aref tmp 2)))
    res))

(defun set-ptr-head (A) 
  (if (aref A 1) (aset A 2 (aref A 0)) (aset A 2 'head)))

(defun set-ptr-tail (A) 
  (if (aref A 1) (aset A 2 (aref A 1)) (aset A 2 'tail)))

(defun ptr-to-tail (A)
  (cond 
    ((not (aref A 2)) (error "pointeur vide"))
    ((eq (aref A 2) 'tail))
    ((eq (aref A 2) 'head) (or (aset A 2 (aref A 0)) (aset A 2 'tail)))
    ((not (aref (aref A 2) 1)) (aset A 2 'tail))
    (t (aset A 2 (aref (aref A 2) 1))))
  A)

(defun ptr-to-head (A)
  (cond 
    ((not (aref A 2)) (error "pointeur vide"))
    ((eq (aref A 2) 'head))
    ((eq (aref A 2) 'tail)  (or (aset A 2 (aref A 1)) (aset A 2 'head)))
    ((not (aref (aref A 2) 2)) (aset A 2 'head))
    (t (aset A 2 (aref (aref A 2) 2))))
  A)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun gp-get-previous-command (str)
  (let (brt brs (br (aref gp-hist-commands 2)))
    (ptr-to-tail gp-hist-commands)
    (setq brt t)
    (while 
      (and 
         brt 
         (not (symbolp (aref gp-hist-commands 2)))
         (setq brs (aref (aref gp-hist-commands 2) 0))
; Si les débuts de chaines sont égaux alors brt vaut nil
         (setq brt 
           (not 
             (string= str (substring brs 0 (min (length str) (length brs)))))))
      (ptr-to-tail gp-hist-commands))
;    (when brt (aset gp-hist-commands 2 br))
; renvoie nil si le début de la chaîne n'est pas trouvé, la chaine sinon
    (and (not brt) brs)))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun gp-get-next-command (str)
  (let (brt brs (br (aref gp-hist-commands 2)))
;    (when (eq br 'tail) (ptr-to-head gp-hist-commands))
    (ptr-to-head gp-hist-commands)
    (setq brt t)
    (while 
      (and 
         brt 
         (not (symbolp (aref gp-hist-commands 2)))
         (setq brs (aref (aref gp-hist-commands 2) 0))
         (setq brt 
           (not 
             (string= str (substring brs 0 (min (length str) (length brs)))))))
      (ptr-to-head gp-hist-commands))
;    (when brt (aset gp-hist-commands 2 br))
    (and (not brt) brs)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun gp-store-line (str)
  (when (>= (length str) 3)
    (cond 
      (;; Do not copy the entry if it is the same as the current one:
       (and 
        (aref gp-hist-commands 0) 
        (string= (aref (aref gp-hist-commands 0) 0) str))
       (ptr-to-head gp-hist-commands))
      (;; Do not copy the entry if it is the same as the current one, case of
       ;; beginning of list::
       (and 
        (eq (aref gp-hist-commands 2) 'tail)
        (aref gp-hist-commands 1) 
        (string= (aref (aref gp-hist-commands 1) 0) str))
       ;; but go back one step in history:
       (ptr-to-head gp-hist-commands))
      (;; Do not copy the entry if it is the same as the current one, case of
       ;; ????::
       (and 
        (not (symbolp (aref gp-hist-commands 2)))
        (string= (aref (aref gp-hist-commands 2) 0) str))
       (ptr-to-head gp-hist-commands))
      (t (add-head str gp-hist-commands)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unused ??
(defun gp-store-current-command (str)
  (when (>= (length str) 3)
    (add-head str gp-hist-commands)
    (aset gp-hist-commands 2 'head)))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun gp-find-beg-of-last-input nil
  (save-excursion
    (goto-char (point-max))
    (re-search-backward gp-prompt-pattern nil t)
    (setq gp-input-start (match-end 0))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun gp-previous-history-search ()
  "Goes down through history ring looking for commands that
started by what is between gp-input-start and (point)."
  (interactive)
  (gp-find-beg-of-last-input) ;;set gp-input-start
  (when (>= (point) gp-input-start)
    (let 
      ( (br (buffer-substring gp-input-start (point-max))) 
        (brs (buffer-substring gp-input-start (point))) (brn (point))
        br1 br2 br3)
      (setq br2 (aref gp-hist-commands 2))
      (setq br1 (gp-get-previous-command brs))
      (setq br3 (aref gp-hist-commands 2))
      (aset gp-hist-commands 2 br2)
      (unless (string= brs br) (gp-store-line br))
      (aset gp-hist-commands 2 br3)
      (delete-region gp-input-start (point-max))
      (goto-char gp-input-start)
      (cond 
        ((not br1) 
          (insert brs)
          (error "End of history list"))
        (t 
          (insert br1) 
          (goto-char brn))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun gp-previous-history ()
  (interactive)
  (gp-find-beg-of-last-input) ;;set gp-input-start
  (when (>= (point) gp-input-start)
    (let 
      ( (br (buffer-substring gp-input-start (point-max))) br1 br2 br3
        (brn (point)))
      (setq br2 (aref gp-hist-commands 2))
      (setq br1 (gp-get-previous-command ""))
      (setq br3 (aref gp-hist-commands 2))
      (aset gp-hist-commands 2 br2)
      (unless (string= "" br) (gp-store-line br))
      (aset gp-hist-commands 2 br3)
      (delete-region gp-input-start (point-max))
      (goto-char gp-input-start)
      (cond 
        ((not br1) 
          (error "End of history list"))
        (t 
          (insert br1) 
          (goto-char (min brn (point-max))))))))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun gp-next-history-search ()
  "Goes up through history ring looking for commands that
started by what is between gp-input-start and (point)."
  (interactive)
  (gp-find-beg-of-last-input) ;;set gp-input-start
  (when (>= (point) gp-input-start)
    (let 
      ( (br (buffer-substring gp-input-start (point-max))) 
        (brs (buffer-substring gp-input-start (point))) (brn (point))
        br1 br2 br3)
      (setq br2 (aref gp-hist-commands 2))
      (setq br1 (gp-get-next-command brs))
      (setq br3 (aref gp-hist-commands 2))
      (aset gp-hist-commands 2 br2)
      (unless (string= brs br) (gp-store-line br))
      (aset gp-hist-commands 2 br3)
      (delete-region gp-input-start (point-max))
      (goto-char gp-input-start)
      (cond 
        ((not br1) 
          (insert brs)
          (error "End of history list"))
        (t 
          (insert br1) 
          (goto-char brn))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun gp-next-history ()
  (interactive)
  
  (gp-find-beg-of-last-input) ;;set gp-input-start
  (when (>= (point) gp-input-start)
    (let 
      ( (br (buffer-substring gp-input-start (point-max))) br1 br2 br3 
        (brn (point)))
      (setq br2 (aref gp-hist-commands 2))
      (setq br1 (gp-get-next-command ""))
      (setq br3 (aref gp-hist-commands 2))
      (aset gp-hist-commands 2 br2)
      (unless (string= "" br) (gp-store-line br))
      (aset gp-hist-commands 2 br3)
      (delete-region gp-input-start (point-max))
      (goto-char gp-input-start)
      (cond 
        ((not br1) 
          (error "End of history list"))
        (t 
          (insert br1) 
          (goto-char (min brn (point-max))))))))

;;;------------------------------------------------------
;;;------------------------------------------------------
;;;------------------------------------------------------
;;; The alternative would be to handle things like in
;;; term-mode:
;;; (defun term-send-up    () (interactive) (term-send-raw-string "\eOA"))
;;; (defun term-send-down  () (interactive) (term-send-raw-string "\eOB"))
;;; (defun term-send-right () (interactive) (term-send-raw-string "\eOC"))
;;; (defun term-send-left  () (interactive) (term-send-raw-string "\eOD"))
;;;------------------------------------------------------

(defun gp-toggle-arrow-behaviour nil
  (interactive)
  (cond
   ((string= gp-arrow-behaviour "Usual")
    (gp-set-arrow-behaviour 'gp-arrow-behaviour "Bash-Style"))
   ((string= gp-arrow-behaviour "Bash-Style")
    (gp-set-arrow-behaviour 'gp-arrow-behaviour "Usual"))
   (t (error "gp-arrow-behaviour has a undefined value")))
  (message (gp-messager 92)))

(add-hook 'gp-mode-hook
  '(lambda nil
     (define-key gp-map [(meta up)] 
      (function gp-previous-history-search))
     (define-key gp-map [(meta down)] 
       (function gp-next-history-search))
     (gp-set-arrow-behaviour nil gp-arrow-behaviour)
     (make-local-variable 'gp-hist-commands)
; gestion de l'historique 
     (setq gp-hist-commands (head-tail-void))
     (set-ptr-head gp-hist-commands)
    ))

(add-hook 'pari-menu-bar-update-hook
  '(lambda nil
     (when (and gp-menu-barp (eq major-mode 'gp-mode) GP-menu-map
                (or (and (featurep 'pari-fontification)
                         (= gp-menu-map-level 3))
                    (and (not (featurep 'pari-fontification))
                         (featurep 'pari-completion)
                         (= gp-menu-map-level 2))))
       (easy-menu-add-item GP-menu-map nil
                           (list (gp-messager 88)
                            (vector (gp-messager 89) 'gp-previous-history)
                            (vector (gp-messager 90) 'gp-next-history)
                            (vector (gp-messager 91) 'gp-toggle-arrow-behaviour)
                            )
                           (gp-messager 78))
       (setq gp-menu-map-level 4)
       (message "Menu bar item GP loaded till level 4."))))

;; pari-history.el ends here ---------------
