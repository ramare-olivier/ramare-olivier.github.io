;; pari-fontification.el -- fontification functions.

;; Copyright (C) 1997-2022  The PARI group.

;; This file is part of the PARIEMACS package.

;; PARIEMACS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation. It is distributed in the hope that it
;; will be useful, but WITHOUT ANY WARRANTY WHATSOEVER.

;; Check the License for details. You should have received a copy of
;; it, along with the package; see the file 'COPYING'. If not, write
;; to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Provides: variable:  gp-fontification-keywords
;;           functions: gp-update-fontification, gp-find-global-var, 

;; To be used with pari.el version 3.00 or higher.
;; pari-fontification.el version 3.20

;; See README for more details.

;;; TODO:         (please feel free to propose !)
;;  -- Introducing three levels of fontification.
;;  -- Introducing predefined combination of colors.
;;     See mupad-color-scheme and mupad-color-scheme-alist.
;;  -- An initial value of gp-fontifyp to nil ...


;; Of pari.el, it uses:
;;     functions: gp-window-manager, gp-store-wind-conf
;; Of pari-messages.el, it uses: functions: gp-messager

(require 'font-lock)
(require 'faces)
(eval-and-compile
(unless (fboundp 'gp-messager)
  (defun gp-messager (no) (print "Feature pari-messages is absent ..." "*Messages*")))
(unless (fboundp 'gp-window-manager)
  (defun gp-window-manager (a b) (message "Main program pari.el is absent !!")))
(unless (fboundp 'gp-info-wind-conf)
  (defun gp-info-wind-conf nil (message "Main program pari.el is absent !!")))
(unless (fboundp 'gp-store-wind-conf)
  (defun gp-store-wind-conf nil (message "Main program pari.el is absent !!"))))

(eval-when-compile
  (require 'font-lock)
  (require 'faces)
  ;(fset 'defined-colors nil)
  ;; for development:
  ;;(setq byte-compile-warnings (list 'free-args 'unresolved 'callargs 'redefine 'obsolete))
  ;; for users:
  (setq byte-compile-warnings (list 'unresolved 'redefine 'obsolete))
  )

(defun gp-update-fontification-buffers nil
"Update (/un)-fonctification on all the buffers
that are in gp-mode or in gp-script-mode."
  (interactive)
  (save-excursion
      (mapcar
      (lambda (abuffer)
              (set-buffer abuffer)
              (if (memq major-mode '(gp-script-mode gp-mode))
                  (if gp-fontifyp (font-lock-fontify-buffer)
                     (font-lock-unfontify-buffer))))
      (buffer-list))
  (message "")))

(defun gp-set-fontifyp (sym val)
  (set sym (and (eq window-system 'x) (x-display-color-p) val))
  (gp-update-fontification-buffers))

(defcustom gp-fontifyp t
"If this variable is nil neither fontify GP scripts nor *PARI* buffer.
Partly modified internally"
:type 'boolean
:set 'gp-set-fontifyp
:initialize 'custom-initialize-reset
:group 'gp-font-lock-and-completion)

(defcustom gp-binds-Clp t
"If this variable is t, C-l is bound to gp-update-fontification
in both gp-mode and gp-script-mode. "
:type 'boolean
:set (lambda (sym val) (setq gp-binds-Clp val))
:initialize 'custom-initialize-reset ;if you use :set, you should specify :initialize!
:group 'gp-font-lock-and-completion)

(eval-and-compile
(mapcar (lambda (agpplace) (eval (list 'defvar (eval agpplace) agpplace)))
     (list
     ''gp-error ''gp-history ''gp-prompt  ''gp-output ''gp-input
     ''gp-help  ''gp-timer   ''gp-comment ''gp-string ''gp-control-statement
     ''gp-default-keywords   ''gp-default-set         ''gp-input-cmd
     ''gp-global-var         ''gp-function-proto      ''gp-function-args
     ''gp-time-word          ''gp-parallel-stuff ''gp-typing)))

(defvar pari-fontification-keywords
  (list
   '("\\<\\(buffersize\\|co\\(lors\\|mpatible\\)\\|debug\\(mem\\)?\\|echo\\|format\\|h\\(elp\\|size\\)\\|logfile\\|output\\|p\\(a\\(risize\\|th\\)\\|r\\(imelimit\\|ompt\\)\\|sfile\\)\\|\\(real\\|series\\)precision\\|simplify\\|strictmatch\\|timer\\|parisizemax\\|nbthreads\\)\\>"  (1 gp-default-keywords))
   '("\\(|\\|<-\\|\\.\\.\\|&\\|->\\)" (0 gp-default-keywords))
  ; '("\\<\\(return\\|break\\|next\\|if\\|until\\|while\\|sum\\|for\\(div\\|prime\\|step\\|vec\\|subgroup\\)?\\)\\>" (1 gp-control-statement))
   '("\\<\\(parforprime\\|parfor\\|parforeach\\|parforprimestep\\|parforvec\\|while\\|until\\|break\\|breakpoint\\|if\\|iferr\\|for\\|forsquarefree\\|forcomposite\\|fordiv\\|fordivfactored\\|foreach\\|forell\\|forfactored\\|forpart\\|forqfvec\\|forperm\\|forprime\\|forprimestep\\|forsquarefree\\|sum\\|prod\\|forstep\\|forsubgroup\\|forsubset\\|forvec\\|return\\|my\\|local\\|next\\)\\>" (1 gp-control-statement))
;(print (regexp-opt '("parforprime" "parfor"  "parforeach"  "parforprimestep"  "parforvec" "while"  "until"   "break"  "breakpoint"  "if"  "iferr" "for"  "forcomposite"  "fordiv"  "fordivfactored"  "foreach"  "forell"  "forfactored" "forpart"  "forperm"  "forprime"  "forprimestep"  "forsquarefree" "forstep"  "forsubgroup"  "forsubset"  "forvec"  "return"  "my"  "local" "next")   'word))
   '("\\<\\(Strchr\\|Strexpand\\|Strprintf\\|Strtex\\|addhelp\\|alarm\\|alias\\|allocatemem\\|apply\\|arity\\|call\\|dbg_down\\|dbg_err\\|dbg_up\\|dbg_x\\|errname\\|error\\|extern\\|externstr\\|fileclose\\|fileextern\\|fileflush\\|fileopen\\|fileread\\|filereadstr\\|filewrite\\|filewrite1\\|fold\\|getabstime\\|getenv\\|getheap\\|getlocalbitprec\\|getlocalprec\\|getrand\\|getstack\\|gettime\\|getwalltime\\|inline\\|input\\|install\\|kill\\|listcreate\\|listinsert\\|listkill\\|listpop\\|listput\\|listsort\\|localbitprec\\|localprec\\|mapdelete\\|mapget\\|mapisdefined\\|mapput\\|print\\|print1\\|printf\\|printp\\|printsep\\|printsep1\\|printtex\\|quit\\|read\\|readstr\\|readvec\\|select\\|self\\|setdebug\\|setrand\\|strchr\\|strexpand\\|strjoin\\|strprintf\\|strsplit\\|strtex\\|strtime\\|system\\|trap\\|type\\|uninline\\|version\\|warning\\|whatnow\\|write\\|write1\\|writebin\\|writetex\\|vecsum\\|vecprod\\)\\>" (0 gp-control-statement))
    '("\\<\\(default\\)(" (1 gp-default-set))
    '("\\<\\(time = \\)[0-9][hmn,0-9 ]* ms\." (1 gp-time-word))
    '("^ *\\\\[a-z].*$" . gp-default-set)
    '(":\\(small\\|int\\|real\\|mp\\|vecsmall\\|vec\\|var\\|pol\\|genstr\\|list\\|gen\\|void\\|bool\\|negbool\\|lg\\|str\\|typ\\|small_int\\|nf\\|bnf\\|bnr\\|ell\\|bell\\|clgp\\|prid\\|gal\\)\\>" . gp-typing)
    ; In the two following ones, in case we meet a list, the separators are also painted...
    '("\\<\\([a-zA-Z]\\w*\\)(\\([^)]*\\)) *=[^=]" (1 gp-function-proto) (2 gp-function-args))
    '("\\<global[ \t]*(\\([^)]*\\))" (1 gp-global-var t))
    '("\\<\\(parsum\\|parvector\\|parselect\\|export\\|exportall\\|unexportall\\|parapply\\|pareval\\|parploth\\|unexport\\)\\>" (1 gp-parallel-stuff))
    )
  "Common keywords to be fontified in gp- and gp-script- mode.")

;(print (regexp-opt '("Strchr" "Strexpand" "Strprintf" "Strtex" "addhelp" "alarm" "alias" "allocatemem" "apply" "arity" "call" "dbg_down" "dbg_err" "dbg_up" "dbg_x" "errname" "error" "extern" "externstr" "fileclose" "fileextern" "fileflush" "fileopen" "fileread" "filereadstr" "filewrite" "filewrite1" "fold" "getabstime" "getenv" "getheap" "getlocalbitprec" "getlocalprec" "getrand" "getstack" "gettime" "getwalltime" "inline" "input" "install" "kill" "listcreate" "listinsert" "listkill" "listpop" "listput" "listsort" "localbitprec" "localprec" "mapdelete" "mapget" "mapisdefined" "mapput" "print" "print1" "printf" "printp" "printsep" "printsep1" "printtex" "quit" "read" "readstr" "readvec" "select" "self" "setdebug" "setrand" "strchr" "strexpand" "strjoin" "strprintf" "strsplit" "strtex" "strtime" "system" "trap" "type" "uninline" "version" "warning" "whatnow" "write" "write1" "writebin" "writetex" "sum" "vecsum" "vecprod" "prod" )  'word))

;(print (regexp-opt '("Strprintf" "addhelp" "alarm" "alias" "allocatemem" "apply" "break" "default" "error" "extern" "for" "fordiv" "forell" "forprime" "forstep" "forsubgroup" "forvec" "getheap" "getrand" "getstack" "gettime" "global" "if" "input" "kill" "local" "my" "next" "print" "print1" "printf" "printtex" "quit" "return" "select" "setrand" "system" "trap" "type" "until" "version"  "warning" "whatnow" "while" "write" "write1" "writebin"   "writetex" "apply" "parapply" "parfor" "parsum" "sum" "vector" "parvector" "inline") 'word))

;(print (regexp-opt '("Strprintf" "addhelp" "alarm" "alias" "allocatemem" "apply" "break" "default" "error" "extern" "for" "fordiv" "forell" "forprime" "forstep" "forsubgroup" "forvec" "getheap" "getrand" "getstack" "gettime" "global" "if" "input" "kill" "local" "my" "next" "print" "print1" "printf" "printtex" "quit" "return" "select" "setrand" "system" "trap" "type" "until" "version"  "warning" "whatnow" "while" "write" "write1" "writebin"   "writetex" "sum" "vector" )   'word))


(defvar gp-fontification-keywords
  (append
  (list
    ;; Careful ! everything involving gp-prompt-pattern
    ;; should be redefined in gp-make-gp-prompt-pattern
    (list gp-prompt-pattern (list 1 'gp-prompt))
    '("^ *%[0-9]* =" .  gp-history)
    '(gp-match-output (0 gp-output t))
    '("\\*\\*\\*.*$" (0 gp-error t))
    '("^[a-zA-Z][a-zA-Z0-9_]*([^ )]*): \\(*\\)$" (1 gp-help))
    '(gp-match-input (0 gp-input))
    '(gp-match-string (0 gp-string t))
    '("time = \\([0-9][hmn,0-9 ]* ms\.\\)"  (1 gp-timer))
    )
   pari-fontification-keywords)
  "Patterns to be fontified under gp-mode.")

(defvar gp-script-fontification-keywords
  (append pari-fontification-keywords
  (list
    '(gp-find-global-var (1 gp-global-var))
    '("\\<read\\(?:vec\\)\\|install\\>" . gp-input-cmd)
    ))
  "Patterns to be fontified under gp-script-mode.")

(defun gp-fontification-switch nil
   (interactive)
   (gp-set-fontifyp 'gp-fontifyp (not gp-fontifyp)))

;;------------------------
;; PART V : HIGHLIGHTING
;;------------------------

(defsubst gp-default-face (gp-face default-face doc)
  (if (not (null (get gp-face 'saved-face)))
      (custom-declare-face gp-face (get 'saved-face default-face) doc)
    (copy-face default-face gp-face)
    (set-face-documentation gp-face doc)))

(defun gp-init-font-lock-faces nil
  "Define gp-faces."
  (interactive)
  (gp-default-face gp-error font-lock-warning-face
                   "*Face used in GP to highlight errors.")
  (gp-default-face gp-comment font-lock-comment-face
                   "*Face used in GP to highlight comments. Default is font-lock-comment-face.")
  (gp-default-face gp-string font-lock-string-face
                   "*Face used in GP to highlight string. Default is font-lock-string-face.")
  (gp-default-face gp-function-proto font-lock-function-name-face
                   "*Face used in GP to highlight function names in definitions.
Default is font-lock-function-name-face.")
  (gp-default-face gp-function-args font-lock-variable-name-face
                   "*Face used in GP to highlight function arguments in definitions.
Default is font-lock-variable-name-face.")
  (gp-default-face gp-global-var font-lock-constant-face
                   "*Face used in GP Script to highlight global variables.
Default is font-lock-constant-face.")
  (gp-default-face gp-history font-lock-constant-face
                   "*Face used in GP to highlight function arguments in definitions.
Default is font-lock-constant-face.")
  (gp-default-face gp-default-keywords font-lock-builtin-face
       "*Face used in GP to highlight some keywords (buffersize, simplify ...).
Default is font-lock-builtin-face.")
  (gp-default-face gp-control-statement font-lock-keyword-face
       "*Face used in GP to highlight control-statements (for, while ...).
Default is font-lock-keywords.")
  (defface gp-prompt
    '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "Orchid" :background "LightCyan" 
                                                       :bold t))
      (((class color) (background dark)) (:foreground "LightSteelBlue" :background "Orchid" 
                                                      :bold t))
      (t (:bold t)))
    "*Face used in GP to highlight prompt.")
  (defface gp-output
    '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
      (((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:italic t)))
    "*Face used in GP to highlight outputs.")
  (defface gp-input
    '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:bold t)))
    "*Face used in GP to highlight inputs.")
  (defface gp-help
    '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "Orchid"))
      (((class color) (background dark)) (:foreground "LightSteelBlue"))
      (t (:bold t)))
    "*Face used in GP to highlight help messages in *PARI* buffer.")
  (defface gp-timer
    '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
      (((class color) (background light)) (:foreground "tomato" 
                                                       :background "snow1" 
                                                       :italic t))
      (((class color) (background dark)) (:foreground "OrangeRed" :background "Orchid"
                                                      :italic t))
      (t (:italic t)))
    "*Face used in GP to highlight time.")
  (defface gp-time-word
    '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
      (((class color) (background light)) (:foreground "tomato" :background "snow1"
                                                       :italic t))
      (((class color) (background dark)) (:foreground "OrangeRed" :background "Orchid"
                                                      :italic t))
      (t (:italic t)))
    "*Face used in GP to highlight the word time for the timer.")
  (defface gp-parallel-stuff
    '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
      (((class color) (background light)) (:foreground "tomato" :background "snow1"
                                                       :italic t))
      (((class color) (background dark)) (:foreground "OrangeRed" :background "Orchid"
                                                      :italic t))
      (t (:italic t)))
    "*Face used in GP to highlight some parallel instructions.")
  (defface gp-typing
    '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
      (((class color) (background light)) (:foreground "tomato" :background "snow1"
                                                       :italic t))
      (((class color) (background dark)) (:foreground "OrangeRed" :background "Orchid"
                                                      :italic t))
      (t (:italic t)))
    "*Face used in GP to highlight type specification for gp2c.")
  (defface gp-default-set
    '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:bold t)))
     "*Face used in GP to highlight default-set.")
  (defface gp-input-cmd
    '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "Orchid"))
      (((class color) (background dark)) (:foreground "LightSteelBlue"))
      (t (:bold t)))
    "*Face used in GP Script to highlight input-cmd (read, install)."))

(defun gp-match-output (limit)
  "Set match-data 0 to limits of next output."
  (if (re-search-forward "^ *%[0-9]* = +" limit t)
      (let ((beg (point)))
        (if (re-search-forward gp-prompt-pattern limit 1)
            (progn (goto-char (match-end 0))
                   (set-match-data (list beg (1- (match-beginning 0))))
                   t)
          nil))
    nil))

(defun gp-in-commentp (safe-place)
  ;; safe-place is a value of point before (point)
  ;; and where we know not to be inside a comment.
  ;; Embedded comments are handled properly.
  (save-excursion
    (let ((p (point)) (profondeur 0))
      (beginning-of-line)
      (or (re-search-forward "\\\\\\\\" p t) ;; within a one-line comment
          (progn
            (goto-char safe-place)
            (while (re-search-forward "/\\*" p t)
              (setq profondeur (+ profondeur 1))
              (while (re-search-forward "\\*/" p t)
                (setq profondeur (- profondeur 1))))
            ;; return value:
            (if (= profondeur 0) t nil))))))

(defun gp-find-global-var-old (limit)
  "A parser to find global variables. Called on a gp-program outside
a function-definition or a comment, gives position via (cons start end) of
next global-variable-definition not surrounded by {} and set the
point at the end of the line. Answer nil if no global-variable is found.
The end delimiter of a function definition surrounded by {} is
'}\n' and the same holds with function definitions of the style
'fun(var)={foo}'.
LIMIT is not used."
  (let ((answer nil) (continue t) (safe-place (point)))
;    (while (looking-at (concat comment-start-skip
;                               "\\|[ \\|\t\\|\n]\\|{\\([^}]}\\|}[^\n]\\)*\n\\|\\<[a-zA-Z]\\w*([^)]*) *={\\([^}]\\|}[^\n]\\)*}\n\\|\\<[a-zA-Z]\\w*([^)]*) *=\\([^=\\\\\"]\\|\\\\[ \t]*\\(\\\\\\\\.*$\\|/\\*\\([^\\*]\\|\\*[^/]\\)*\\*/\\)?\n\\|\"\\([^\"]*\\|\\\\\"\\)*\"\\)\\([^\\\\\n\"]\\|\"\\([^\"]*\\|\\\\\"\\)*\"\\|\\\\[ \t]*\\(\\\\\\\\.*$\\|/\\*\\([^\\*]\\|\\*[^/]\\)*\\*/\\)?\n\\)*\n\\|\\<[a-zA-Z]\\w*([^)]*)[;\n]"))
  ;; We look at a single line comment, or a long comment,
  ;; or a space/tab/newline character, 
  ;; or a function definition of the type fun(var)={foo},
  ;; or a function definition of the type {fun(var)= foo},
  ;; (that is, the final } is immediately followed by a \n or by ;\n)
  ;; or a function call,
  ;; or any line without an equality sign.
  ;; And skip them.
;      (goto-char (match-end 0)))

    (while continue
      (setq continue nil)
      (skip-chars-forward " \t\n")
      (while 
          (or
           (looking-at (concat comment-start-skip
                               "\\|\\<[a-zA-Z]\\w*([^)]*) *=\\([^=\\\\\"]\\|\\\\[ \t]*\\(\\\\\\\\.*$\\|/\\*\\([^\\*]\\|\\*[^/]\\)*\\*/\\)?\n\\|\"\\([^\"]*\\|\\\\\"\\)*\"\\)\\([^\\\\\n\"]\\|\"\\([^\"]*\\|\\\\\"\\)*\"\\|\\\\[ \t]*\\(\\\\\\\\.*$\\|/\\*\\([^\\*]\\|\\*[^/]\\)*\\*/\\)?\n\\)*\n"))
           (looking-at "\\<[a-zA-Z]\\w*([^)]*) *= *{\\([^}]\\|}[^\n]\\)*}[;]*\n")
           (looking-at "{[ \n\t]*\\<[a-zA-Z]\\w*([^)]*) *= *\\([^}]\\|}[^\n]\\|};[^\n]\\)*}[;]*\n")
           (looking-at "\\<[a-zA-Z]\\w*([^)]*)[;\n]")
           )
        ;; We look at a single line comment, or a long comment,
        ;; or a space/tab/newline character, 
        ;; or a function definition of the type fun(var)={foo},
        ;; or a function definition of the type {fun(var)= foo},
        ;; (that is, the final } is immediately followed by a \n)
        ;; or a function call,
        ;; or any line without an equality sign.
        ;; And skip them.
        (setq continue t)
        (goto-char (match-end 0)))
      (setq safe-place (point))
      ;; Skipping things enclosed in { / }\n :
      (when (looking-at "{")
        (while (and (re-search-forward "}\n" nil t)
                    (save-excursion
                      (backward-char 2)
                      (not (gp-in-commentp safe-place)))))
        (backward-char 2)
        (setq continue (looking-at "}\n"))
        (forward-char 2)))

  ;; We look whether there is a global-variable being defined here:
    (if (looking-at "^\\([a-zA-Z]\\w*\\)[ \t]*=[^=].*$")
        (progn
          (setq answer (cons (match-beginning 1) (match-end 1)))
          (goto-char (match-end 0))))
    answer))

(defun gp-find-global-var (limit)
  "A parser to find global variables. Called on a gp-program outside
a function-definition or a comment, gives position via (cons start end) of
next global-variable-definition and sets the point at the end of the line.
Answer nil when no global-variable is found"
  (let ((answer nil))
    (if (re-search-forward "^\\([a-zA-Z]\\w*\\)[ \t]*=[^=].*$" limit t)
      (progn
        (setq answer (cons (match-beginning 1) (match-end 1)))
        (goto-char (match-end 0))))
    answer))

(defun gp-match-string (lim)
  "Match next quoted string \"\" of `gp-mode-output' up to LIM.
This function is designed as a matcher for `font-lock-keywords'.

If a string is found then set the match data to it, including the
quotes, move point after, and return point.
If no string is found up to LIM then return nil.

Only strings with `gp-mode-output' text property are matched, and
they cannot cross over non-`gp-mode-output' text.

Backslash escapes in strings are understood, so \\=\\\" does not
end a string, and \\=\\\\=\\ is a literal backslash.  gp prints
string results in this form."

  ;; A string must start before LIM but the end can be later.  Per the elisp
  ;; manual under `font-lock-multiline', a matcher function should give the
  ;; whole construct even if only part is to be font locked.  Usually
  ;; multi-line font lock is supposed to extend its region so LIM is not in
  ;; the middle of a multi-line construct.  We probably don't have that
  ;; setup correctly yet, but for now gp-update-fontification does a full
  ;; buffer update so it doesn't matter.

  ;; Inspired from fontify-string-find in hilit19.el.
  (let (beg found) 
    (while (and (setq beg (gp-search-doublequote lim)) ;; after open "
                ;(print (list "Beg" beg (text-property-any (1- beg) beg 'gp-mode-output t)))
                ;; Want open quote to be in gp-mode-output, and want a close
                ;; quote within this gp-mode-output.  If not keep looping.
                (not (and (get-text-property (1- beg) 'gp-mode-output)
                          (setq found (gp-search-doublequote
                                       (next-single-property-change
                                        (1- beg) 'gp-mode-output)))))))
    (when found
      ;;(print "Setting match-data")
      (set-match-data (list (1- beg) (point)))
      (point))))

(defun gp-search-doublequote (&optional lim)
  "Search forward for an unbackslashed \" at or after point.
If found then move point after it and return point.
If not found then return nil, possibly with point moved somewhere
forward.

A \" can have any even number of backslashes preceding, including
no backslashes.  Any odd number of backslashes \\=\\\" is
considered backslashed and not returned."

  (let (found)
    (while (and (search-forward "\"" lim t)
                (save-excursion
                  (goto-char (1- (point)))
                  (let ((aux (point)))
                    (skip-chars-backward "\\\\")
                    ;; want even number of backslashes, if not keep looping
                    (not (setq found (zerop (% (- aux (point)) 2))))))))
    (and found
         (point))))

(defun gp-update-fontification nil "Update fontification."
  (interactive)
  (when gp-fontifyp ;;(print "Yo")
    (font-lock-fontify-buffer)
    (font-lock-fontify-region (point-min) (point-max) t) ;; TO BE REMOVED!!
    (font-lock-flush)))

(defun gp-turn-on-lazy-font-lock nil ""
  (interactive)
  (require 'lazy-lock)
  (when (featurep 'lazy-lock) (lazy-lock-mode)))

(defun gp-customize-faces nil
  (interactive)
  (if (not (fboundp 'defined-colors))
      (message "No colours found !!!")
    ;; Don't ask me why the following line is required !!
    (fset 'x-defined-colors (symbol-function 'defined-colors))
    (if gp-tutorial-requiredp
        (let ((wind (selected-window)) s
              (msg (gp-messager 28))
              (colors-list (sort (defined-colors) 'string-lessp)))
          (gp-window-manager "*gp-help*" 'gp-beginning-temp)
          (insert msg)
          (fill-region (point-min) (point-max) 'left)
          ;; Following taken from list-colors-display of facemenu.el:
          (while colors-list
            (setq s (point))
            (insert (car colors-list))
            (indent-to 20)
            (put-text-property s (point) 'face 
                               (cons 'background-color (car colors-list)))
            (setq s (point))
            (insert "  " (car colors-list) "\n")
            (put-text-property s (point) 'face 
                               (cons 'foreground-color (car colors-list)))
            (setq colors-list (cdr colors-list)))
          (goto-char (point-min))
          (gp-info-wind-conf)
          (select-window wind)))
    (customize-apropos-faces "font-lock-.*\\|gp-.*")))

(defun gp-customize-gp-group nil
  (interactive)
  (gp-store-wind-conf)
  (customize-group "gp")
  (message (gp-messager 4)))

(defun gp-color-menu nil
  "Build the Colors menu"
  (when (and  gp-menu-barp (eq window-system 'x) (x-display-color-p))
     (append
      (if (eq major-mode 'gp-script-mode)
          (list (vector (gp-messager 45) 'gp-turn-on-lazy-font-lock
                        ':active t ':key-sequence nil 
                        :label     '(gp-messager 45)
                        ':included '(and gp-fontifyp (eq major-mode gp-script-mode)))) nil)
      (list (vector (gp-messager 44) 'gp-update-fontification
                    :label   '(gp-messager 44)
                    ':active t ':included: 'gp-fontifyp)
            (vector (gp-messager 46) 'gp-fontification-switch
                    :label   '(gp-messager 46)
                    ':active t ':key-sequence nil ':included: 'gp-fontifyp)
            (vector (gp-messager 47) 'gp-update-fontification-buffers
                    :label   '(gp-messager 47)
                    ':active t ':key-sequence nil)
            (vector (gp-messager 79) 'gp-customize-faces ':active t
                    :label '(gp-messager 79)
                    ':key-sequence nil ':included: 'gp-fontifyp)))))

(add-hook 'pari-menu-bar-update-hook
  '(lambda nil 
     (when (and gp-menu-barp
                (or (and (eq major-mode 'gp-mode) GP-menu-map (= gp-menu-map-level 2))
                    (and (eq major-mode 'gp-script-mode) 
                         GP-script-menu-map (= gp-script-menu-map-level 2) )))
       (let ((sentences (if (and (require 'pari-completion) (featurep 'pari-completion))
                    ;; (gp-messager 72) [Complete] is defined there.
                       (list (gp-messager 72) "")
                     (list (gp-messager 48)  " (level 2 skipped)"))))
         (when (eq major-mode 'gp-mode)
           (easy-menu-change '("GP") (gp-messager 43) (gp-color-menu) (car sentences))
           (easy-menu-add-item GP-menu-map nil gp-separator (car sentences))
           (setq gp-menu-map-level 3)
           (message (concat "Menu bar item GP loaded till level 3" (cadr sentences) ".")))
         (when (eq major-mode 'gp-script-mode)
           (easy-menu-change '("GP-script") (gp-messager 43) (gp-color-menu) (car sentences))
           (easy-menu-add-item GP-script-menu-map nil gp-separator (car sentences))
           (setq gp-script-menu-map-level 3)
           (message (concat "Menu bar item GP-script loaded till level 3" (cadr sentences) ".")))))))

(add-hook 'pari-mode-hook
  '(lambda nil
     (when gp-binds-Clp
       (define-key gp-map        "\C-l" (function gp-update-fontification))
       (define-key gp-script-map "\C-l" (function gp-update-fontification)))
     (require 'font-lock)
     (gp-init-font-lock-faces)))

(add-hook 'gp-script-mode-hook
  '(lambda nil
     (make-local-variable 'font-lock-defaults)
     (make-local-variable 'font-lock-comment-face)
     (setq font-lock-comment-face (eval gp-comment))
     (make-local-variable 'font-lock-string-face)
     (setq font-lock-string-face (eval gp-string))
     (make-local-variable 'font-lock-keywords-only)
     (setq font-lock-defaults '(gp-script-fontification-keywords nil nil nil))
     ))

(add-hook 'gp-mode-hook
  '(lambda nil
     (make-local-variable 'font-lock-defaults)
     (make-local-variable 'font-lock-comment-face)
     (setq font-lock-comment-face (eval gp-comment))
     (make-local-variable 'font-lock-string-face)
     (setq font-lock-string-face (eval gp-string))
     (make-local-variable 'font-lock-keywords-only)
     (setq font-lock-defaults '(gp-fontification-keywords t nil nil))
     ))

(provide 'pari-fontification)

;; pari-fontification.el ends here.
