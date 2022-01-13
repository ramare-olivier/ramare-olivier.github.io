;; pari-help.el -- documentation functions.

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

;; To be used with pari.el version 3.00 or higher
;; pari-help.el version 3.20 (10-January-2022)

;; See README for more details.
 
(provide 'pari-help)

;; pari.el will use the variable 'gp-c-array-createdp
;; and the functions 'gp-cpl-init, 'gp-menu-quit.
;; Also extends pari-mode-hook to set some key-bindings.

;; Of pari.el, it uses:
;; variables:
;;     gp-file-name, gp-version
;; functions: 
;;     gp-window-manager, gp-wait-for-output, gp-get-shell,
;;     gp-background, gp-meta-cmd-general,
;;     gp-info-wind-conf, gp-add-symbol, gp-backward-wind-conf, gp-show-help
;; Of pari-messages.el, it uses: functions: gp-messager.
;; Of pari-completion.el, it uses:
;;     functions: gp-find-word-to-complete, gp-add-symbol, gp-show-help
;;     variables: gp-c-array.
(eval-when-compile
  ;(fset 'defined-colors nil)
  ;; for development:
  ;;(setq byte-compile-warnings (list 'free-args 'unresolved 'callargs 'redefine 'obsolete))
  ;; for users:
  (setq byte-compile-warnings (list 'unresolved 'redefine 'obsolete))
  )
(eval-and-compile
(unless (fboundp 'gp-messager)
  (defun gp-messager (no) (print "Feature pari-messages is absent." "*Messages*")))
;;
(unless (fboundp 'gp-info-wind-conf)
  (defun gp-info-wind-conf nil (message "Main program pari.el is absent !!")))
(unless (fboundp 'gp-store-wind-conf)
  (defun gp-store-wind-conf nil (message "Main program pari.el is absent !!")))
(unless (fboundp 'gp-backward-wind-conf)
  (defun gp-backward-wind-conf nil (message "Main program pari.el is absent !!")))
(unless (fboundp 'gp-restore-wind-conf)
  (defun gp-restore-wind-conf nil (message "Main program pari.el is absent !!")))
(unless (fboundp 'gp-background)
  (defun gp-background nil (message "Main program pari.el is absent !!")))
(unless (fboundp 'gp-wait-for-output)
  (defun gp-wait-for-output (pt &optional o p n) (message "Main program pari.el is absent !!")))
(unless (fboundp 'gp-get-shell)
  (defun gp-get-shell (pn pb cmd) (message "Main program pari.el is absent !!")))
(unless (fboundp 'gp-meta-cmd-general)
  (defun gp-meta-cmd-general (cmd wo) (message "Main program pari.el is absent !!")))
;;
(unless (fboundp 'gp-find-word-to-complete)
  (defun gp-find-word-to-complete nil (print "Feature pari-completion is absent." "*Messages*")))
(unless (fboundp 'gp-show-help)
  (defun gp-show-help (s) (print "Feature pari-completion is absent." "*Messages*")))
(unless (fboundp 'gp-add-symbol)
  (defun gp-add-symbol (s) (print "Feature pari-completion is absent." "*Messages*"))))

(defvar gp-c-array nil "Defined in pari-completion.el")
;; The next two variables are here to pacify the compiler !
;; Do *not* assign any value to them or they may override ....
(defvar gp-file-name)
(defvar gp-version)
;;; help menu part:

;; Topology of a menu-buffer : three parts delimited
;; by 'gp-menu-start-simple/'gp-menu-end-simple
;;    'gp-menu-start-special/'gp-menu-end-special
;;    'gp-menu-start-keywords/'gp-menu-end-keywords
;; The first part is made of items displayed on gp-menu-nbcol columns
;; of width gp-menu-width; selecting an item in this region will
;; ask 'gp-get-man-entry. The second part is made of longer items
;; displayed on a single columns and selecting them will also
;; call 'gp-get-man-entry. The third part is made of keywords
;; displayed on a single column which when selected will call
;; 'gp-get-apropos.
(defvar gp-menu-start-simple 0
"Value of point at the beginning of the first menu-region")
(defvar gp-menu-end-simple 0
"Value of point at the end of the first menu-region")
(defvar gp-menu-width 1)
(defvar gp-menu-nbcol 1)
(defvar gp-menu-start-special 0
"Value of point at the beginning of the second menu-region")
(defvar gp-menu-end-special 0
"Value of point at the end of the second menu-region")
(defvar gp-menu-start-keywords 0
"Value of point at the beginning of the third menu-region")
(defvar gp-menu-end-keywords 0
"Value of point at the end of the third menu-region")

(mapcar 'make-variable-buffer-local
  '(gp-menu-start-simple gp-menu-end-simple
    gp-menu-start-special gp-menu-end-special gp-menu-start-keywords
    gp-menu-end-keywords))

(defvar gp-menu-map nil
  "Local keymap used in gp menu buffer.")

(when (null gp-menu-map)
(let ((map (make-sparse-keymap)))
(define-key map "\C-n"    (function gp-menu-next))
(define-key map "\C-p"    (function gp-menu-previous))
(define-key map "\C-m"    (function gp-menu-select))
(define-key map [mouse-2] (function gp-menu-mouse-select))
(define-key map "q"       (function gp-menu-quit))
(define-key map "\C-v"    (function gp-menu-C-v))
(define-key map "\M-v"    (function gp-menu-M-v))
(define-key map [right]   (function gp-menu-right))
(define-key map [left]    (function gp-menu-left))
(setq gp-menu-map map)))

(defvar gp-c-array-createdp nil
"t if the file gp-menu is already used for completion")

;;; Browser part:
(defvar gp-browser-2-map nil "Keymap used in `gp-browser-2-mode'.")

(when (null gp-browser-2-map)
(setq gp-browser-2-map (make-sparse-keymap))
(define-key gp-browser-2-map "\C-m"     (function gp-browser-2-select))
(define-key gp-browser-2-map [mouse-2]  (function gp-browser-2-mouse-select))
(define-key gp-browser-2-map [right]    (function gp-menu-right))
(define-key gp-browser-2-map [left]     (function gp-menu-left)))

(defvar gp-browser-1-map nil "Keymap used in `gp-browser-1-mode'.")

(when (null gp-browser-1-map)
(setq gp-browser-1-map (make-sparse-keymap))
(define-key gp-browser-1-map "\C-m"    (function gp-browser-1-select))
(define-key gp-browser-1-map [mouse-2] (function gp-browser-1-mouse-select)))

(defvar gp-browser-main-alist nil)
(defvar gp-browser-width 0
"The alist `gp-browser-main-alist' consists of conses (NUM . STRING).
`gp-browser-width' is the length of the largest STRING.")

(defvar gp-browser-follow-alist nil)
(defvar gp-main-frame nil "The main frame if non nil.")
(defvar gp-browser-frame nil "GP-browser frame if non nil.")
(defvar gp-browser-process nil "A simple gp process opened to build the browser.")
(defcustom gp-browser-style 3
"*Style for GP-browser. Three styles are possible, 1, 2 and 3.
Best to try them on."
:type 'integer   :group 'gp-miscellana)

(defvar gp-functions-list-buffer-name " *gp-functions list*")
(defvar gp-function-description-buffer-name " *gp-function description*")
(defvar gp-browser-buffer-name " *gp-browser*")
(defvar gp-pari-simple-buffer-name " *Simple PARI*")
;;--------------
;; GP HELP MODE
;;--------------

(defsubst gp-has-spacep (word)
  "T if WORD contains a space and NIL otherwise"
  (not (eq (memq ?\  (string-to-list word)) nil)))

(defun gp-display-raw-menu (lst start end)
  (beginning-of-line)
  (set start (point))
  (save-excursion
    (mapcar
      (lambda (astring)
        (insert astring "\n")
        (put-text-property (- (1- (point)) (length astring)) (1- (point))
                           'mouse-face 'highlight))
      lst)
    (set end (point))
    (insert "\n")))

(defsubst gp-display-special-menu (lst)
  (gp-display-raw-menu lst 'gp-menu-start-special 'gp-menu-end-special))

(defsubst gp-display-keywords-menu (lst)
  (gp-display-raw-menu lst 'gp-menu-start-keywords 'gp-menu-end-keywords))

(defun gp-split-menu (lst)
  (let ((lst-simple nil) (lst-special nil))
    (mapcar
      (lambda (astring)
        (add-to-list (if (gp-has-spacep astring)
                         'lst-special
                         'lst-simple) astring))
      lst)
    (list lst-simple lst-special)))

(defun gp-display-simple-menu (lst)
  "Display the list of strings LST on several columns and sets
the values of `gp-menu-start-simple', `gp-menu-end-simple'."
  (let*((length-list (sort (mapcar 'length lst) '>))
        (how-many (length lst))
        (gp-menu-width (car length-list))  ; maximal width
        gp-menu-main-width                 ; usual width + spaces
        (gp-menu-percent-exceptions 30)    ; maximal percentage of exceptionnal words
        (gp-menu-spaces 2)                 ; spaces between two columns
        (where (max 0 (1- (floor (* how-many gp-menu-percent-exceptions) 100)))))
       ;; Exceptionnal items will be spread over two columns.

       ;; There may be too many elements of exceptional width:
       (if (and (< (1+ where) how-many)
                (= (nth (1+ where) length-list) (nth where length-list)))
           (while (and (<= where 0)
                       (= (nth (1+ where) length-list) (nth where length-list)))
                  (setq where (1- where))))
       (if (> 0 where) (setq where 0))
       ;; The exceptional width may be too large:
       (while (and (<= where 0)
                   (< (+ gp-menu-spaces (* 2 (nth where length-list))) gp-menu-width))
              (setq where (1- where)))
       (if (> 0 where) (setq where 0))
       (setq gp-menu-main-width (nth where length-list))

       ;; Add some spaces between columns:
      (setq gp-menu-main-width (+ gp-menu-spaces gp-menu-main-width))
      ;; Compute 'gp-menu-nbcol:

      (if (< how-many (/ (window-height) 2))
          (setq gp-menu-nbcol 1)
          (setq gp-menu-nbcol (max 1 (floor (- (window-width) gp-menu-spaces) gp-menu-main-width))))
      (if (= gp-menu-nbcol 1)
          (setq gp-menu-main-width (+ gp-menu-spaces gp-menu-width)))

      ;; Display the list:
      (beginning-of-line)
      (setq gp-menu-start-simple (point))
      (save-excursion
      (let ((wherex 1))
        (mapcar
          (lambda (astring)
            (if (<= (length astring) (- gp-menu-main-width gp-menu-spaces))
                (progn
                   (insert astring)
                   (put-text-property (- (point) (length astring)) (point)
                                      'mouse-face 'highlight)
                   (if (< wherex gp-menu-nbcol)
                       (progn (setq wherex (1+ wherex))
                              (insert-char ?\  (- gp-menu-main-width (length astring))))
                       (setq wherex 1)
                       (insert "\n")))
              (if (< wherex gp-menu-nbcol)
                  (progn
                    (insert astring)
                    (put-text-property (- (point) (length astring)) (point)
                                       'mouse-face 'highlight)
                    (if (< wherex (1- gp-menu-nbcol))
                        (progn (setq wherex (+ 2 wherex))
                               (insert-char ?\  (- (* 2 gp-menu-main-width) (length astring))))
                        (setq wherex 1)
                        (insert "\n")))
                (insert "\n" astring)
                (setq wherex 1)
                (put-text-property (- (point) (length astring)) (point)
                                   'mouse-face 'highlight)
                (if (< wherex (1- gp-menu-nbcol))
                    (progn (setq wherex (+ 2 wherex))
                           (insert-char ?\  (- (* 2 gp-menu-main-width) (length astring))))
                    (setq wherex 1)
                    (insert "\n")))))
          lst)
        (or (= wherex 1) (insert "\n")))
      (message (gp-messager 38))
      (setq gp-menu-end-simple (point)))))
        
(defun gp-menu nil
  "Major-mode for the gp menu buffer.
The available commands are
\\{gp-menu-map}"
  (interactive)
  (gp-window-manager "*gp-menu*" 'gp-beginning)
  (setq major-mode 'gp-menu mode-name "GP MENU")
  (use-local-map gp-menu-map))

(defsubst gp-menu-info nil
  (message (gp-messager 18)))

(defun gp-menu-next ()
  "Move down one line of the gp help menu. (Go to top if at the end.)"
  (interactive)
  (gp-menu-info)
  (forward-line 1)
  (if (eobp)
    (progn (ding)
           (goto-char (point-min))
           (re-search-forward "###\n" nil t))))

(defun gp-menu-previous ()
  "Move up one line of the gp help menu. (Go to bottom if at the top.)"
  (interactive)
  (gp-menu-info)
  (forward-line -1)
  (if (or (bobp) (looking-at "###\n"))
      (progn (ding) (goto-char (point-max)) (beginning-of-line))))

(defun gp-menu-C-v nil (interactive) (scroll-up) (gp-menu-info))

(defun gp-menu-M-v nil (interactive) (scroll-down) (gp-menu-info))

(defun gp-menu-right nil
  (interactive)
  (if (and (> (point) (1- gp-menu-start-simple))
           (< (point) gp-menu-end-simple))
      ;; multiple columns display:
      (progn
        (if (re-search-forward "[\n\t ][a-zA-Z]" nil t)
            (forward-char -1)))
      ;; single column display:
      (forward-char 1)))

(defun gp-menu-left nil
  (interactive)
  (if (and (> (point) (1- gp-menu-start-simple))
           (< (point) gp-menu-end-simple))
      ;; multiple columns display:
      (progn
        (if (re-search-backward "\\([\n\t ]\\|^\\)\\([a-zA-Z]+\\)[\n\t ]" nil t)
            (goto-char (match-beginning 2))))
      ;; single column display:
      (forward-char -1)))

(defsubst gp-menu-get-beg nil
  (save-excursion
    (re-search-backward "\\`\\|[ \n]" nil t)
    (match-end 0)))

(defsubst gp-menu-get-end nil
  (save-excursion
    (re-search-forward "\\'\\|[ \n]" nil t)
    (match-beginning 0)))

(defun gp-menu-select nil
  "Select a subject from the main menu, or a manual entry from a subject menu."
  (interactive)
  (cond ((and (> (point) (1- gp-menu-start-simple))
              (< (point) gp-menu-end-simple))
         (gp-get-man-entry
           (buffer-substring-no-properties (gp-menu-get-beg) (gp-menu-get-end))))
        ((and (> (point) (1- gp-menu-start-special))
              (< (point) gp-menu-end-special))
         (gp-get-man-entry
           (buffer-substring-no-properties (line-beginning-position)
                             (line-end-position))))
        ((and (> (point) (1- gp-menu-start-keywords))
              (< (point) gp-menu-end-keywords))
         (gp-get-apropos
           (buffer-substring-no-properties (line-beginning-position)
                             (line-end-position))))
        (t (message (gp-messager 19)))))

(defun gp-menu-mouse-select (event)
  (interactive "e")
  (mouse-set-point event)
  (gp-menu-select))

;;; We start the browser.

(defun gp-browser-cmd (cmd to-buffer &optional nomessage slow-down)
  ;; A GP process is supposedly running.
  (set-buffer gp-pari-simple-buffer-name)
  (goto-char (point-max))
  ;; Make gp send text to the buffer end, so we can move it to-buffer.
    (set-marker (process-mark gp-browser-process) (point))
    (let ((temp (point)))
      ;; Send the meta command to gp.
      (process-send-string gp-browser-process (concat cmd "\n"))
      ;; Wait for the gp-prompt to be sent.
      (gp-wait-for-output temp nomessage gp-browser-process)
      (if slow-down (sit-for 0 slow-down))

      (let ((copy (buffer-substring-no-properties temp (point-max))))
        (delete-region temp (point-max))
        (set-buffer (get-buffer-create to-buffer))
        (erase-buffer)
        (insert copy)
        (beginning-of-line)  ; We remove last prompt line.
        (delete-region (point) (point-max))
        (goto-char (point-min)))))

(defun gp-browser-1-mode nil ""
  (interactive)
  (setq major-mode 'gp-browser-1)
  (use-local-map gp-browser-1-map))

(defun gp-browser-2-mode nil ""
  (interactive)
  (setq major-mode 'gp-browser-2)
  (setq gp-menu-start-simple (point-min))
  (use-local-map gp-browser-2-map))

(defun gp-browser-1-select nil
  (interactive)
  (let ((be (line-beginning-position))
        (en (line-end-position)))
    (put-text-property (point-min) be 'face 'bold)
    (put-text-property be en 'face 'gp-browser-face)
    (put-text-property en (point-max) 'face 'bold))
  (let ((prop (get-text-property (point) 'follow)))
    (if (numberp prop) (gp-browser-follow prop))))

(defun gp-browser-1-mouse-select (event)
  (interactive "e")
  (mouse-set-point event)
  (gp-browser-1-select))

(defun gp-browser-2-select nil
  (interactive)
  (with-current-buffer gp-function-description-buffer-name
    (erase-buffer))
  (if (or (eobp) (looking-at "[ \n\t]+")) (skip-chars-backward " \n\t"))
  (gp-call-gphelp-olist (window-width (get-buffer-window gp-function-description-buffer-name))
                        (thing-at-point 'word)
                        gp-function-description-buffer-name
                        '("-detex")))

(defun gp-browser-2-mouse-select (event)
  (interactive "e")
  (mouse-set-point event)
  (gp-browser-2-select))

(defun gp-browser-follow (num)
  (set-buffer gp-functions-list-buffer-name)
  (select-window (get-buffer-window gp-functions-list-buffer-name))
  (erase-buffer)
  (gp-display-simple-menu (cdr (assq num gp-browser-follow-alist)))
  (setq gp-menu-end-simple (point-max)))

(defun gp-browser-main nil
  (setq major-mode 'gp-browser-1-mode)
  (mapcar (lambda (acons)
             (let ((temp (point)))
                  (insert (cdr acons))
                  (put-text-property temp (point) 'follow (car acons))
                  (put-text-property temp (point) 'face 'bold)
                  (put-text-property temp (point) 'mouse-face 'highlight)    
                  (insert "\n")))  gp-browser-main-alist)
  (set-buffer-modified-p nil)
  (goto-char (point-min)))

(defface gp-browser-face
  '((t (:underline t :italic t)))
  "*Face used for highlit entries for the browser in GP")

(defsubst gp-browser-common nil
  (setq gp-browser-frame
        (make-frame (list '(name . "gp-browser")
                          (cons 'width (frame-width gp-main-frame))
                          '(line-spacing . 2) ;; in pixel
                          '(cursor-color . "red")
                          '(cursor-type . bar)
                          '(foreground-color . "black")
                                        ;'(background-color . "CadetBlue1")
                          '(background-color . "lightblue")
                          )))
  (select-frame gp-browser-frame)
  (switch-to-buffer gp-browser-buffer-name)
  (gp-browser-1-mode)
  (erase-buffer))

(defun gp-browser nil
  (interactive)
  (or gp-browser-main-alist
      (gp-make-browser))
  (setq gp-main-frame (selected-frame))
  (cond ((= gp-browser-style 1)
         (gp-browser-common)
         (split-window nil (1+ (length gp-browser-main-alist)))
         (other-window 1)
         (switch-to-buffer gp-functions-list-buffer-name)
         (gp-browser-2-mode)
         (split-window nil 12)
         (other-window 1)
         (switch-to-buffer gp-function-description-buffer-name))
        ((= gp-browser-style 2)
         (gp-browser-common)
         (split-window nil (+ 5 (length gp-browser-main-alist)))
         (other-window 1)
         (switch-to-buffer gp-function-description-buffer-name)
         (select-window (get-buffer-window gp-browser-buffer-name))
         (split-window nil (+ 4 gp-browser-width) t)
         (other-window 1)
         (switch-to-buffer gp-functions-list-buffer-name)
         (gp-browser-2-mode))
        ((= gp-browser-style 3)
         (gp-browser-common)
         (split-window nil (+ 4 gp-browser-width) t)
         (other-window 1)
         (switch-to-buffer gp-functions-list-buffer-name)
         (gp-browser-2-mode)
         (other-window -1)
         (split-window nil (1+ (length gp-browser-main-alist)))
         (other-window 1)
         (switch-to-buffer gp-function-description-buffer-name)))
  (set-buffer gp-browser-buffer-name)
  (select-window (get-buffer-window gp-browser-buffer-name))
  (gp-browser-main)
  ;; emacs 19.7 : (set-default-font (frame-parameter gp-main-frame 'font))
  (set-frame-font (frame-parameter gp-main-frame 'font))
  (select-frame gp-main-frame))

(defsubst gp-split-to-strings (to)
  (let ((res nil))
    (while (re-search-forward "\\([^ \n\t]+\\)\\( \\|\n\\|\t\\)" to t)
      (setq res (nconc res (list (match-string 1)))))
    res))

(defun gp-make-browser nil
  ;; Make 'gp-browser-main-alist:
  (message (gp-messager 37))
  (get-buffer-create gp-pari-simple-buffer-name)
  (set-buffer gp-pari-simple-buffer-name)

  ;; run "gp -f" factory settings since don't need extra functions the user
  ;; might load from .gprc for interactive use.  "-p 10" avoids building a
  ;; table of primes.  (Maybe gp 2.7 up doesn't pre-build anyway?)
  ;; Previously had "-s 10000" for small stack, but don't want to hard-code
  ;; a size here.  Let's hope gp has a sensible default (and hopefully into
  ;; uninitialized virtual memory anyway).
  (let ((process-connection-type t)) ; use PTY.
    (setq gp-browser-process
          (gp-get-shell "simple-pari" gp-pari-simple-buffer-name 
                        (concat gp-file-name " -f -p 10 " gp-flag-for-emacs))))
  (set-process-query-on-exit-flag gp-browser-process nil)

  ;; We should run the hook as the prompt may have
  ;; been changed in the .gprc:
  (run-hooks 'pari-mode-hook)
  (gp-wait-for-output 1 t gp-browser-process)
  (gp-browser-cmd "?" gp-browser-buffer-name t)
  (goto-char (point-min))
  (forward-line 2) ; Skip item 0.
  (while (re-search-forward " +\\([1-90]+\\): \\([^\n]*\\)\n" nil t)
    (setq gp-browser-main-alist
       (nconc gp-browser-main-alist
             (list (cons (string-to-number (match-string 1)) (match-string 2))))
       gp-browser-width (max gp-browser-width (length (match-string 2)))))
  ;; Remove last item :
  (setq gp-browser-main-alist (nreverse (nthcdr 1 (nreverse gp-browser-main-alist))))
  ;; Make 'gp-browser-follow-alist:
  (setq gp-browser-follow-alist
   (mapcar
     (lambda (num)
       (erase-buffer)
       (gp-browser-cmd (concat "?" (number-to-string num)) gp-browser-buffer-name t 300)
       (cons num (gp-split-to-strings (point-max))))
     (mapcar 'car gp-browser-main-alist)))
  (gp-browser-cmd "\q" gp-browser-buffer-name t)
  (kill-process gp-browser-process)
  (sit-for 0)
  (set-buffer-modified-p nil)
  (kill-buffer gp-pari-simple-buffer-name)
  (message (gp-messager 6)))

;;--------------------
;; TeX AND USUAL INFO
;;--------------------

(defvar gp-gphelp-coding 'detect
  "Coding system to use with gphelp.
`utf-8' means use \"gphelp -utf8\".
`ascii' means don't use that option, just ASCII output.
`detect' means detect whether gphelp has -utf8 flag (Pari/GP
2.8.x up) and the Emacs display can show relevant characters.

gphelp uses small amounts of utf-8, generally just accents on
people's names.  This variable is slightly experimental, but the
intention would be to allow some sort of user preference for the
output style.")

(defun gp-call-gphelp-olist (win-size word output-buffer opt-list)
  "Run the gphelp program to find information about WORD.
WIN-SIZE (an integer) becomes the COLUMNS environment variable.
OUTPUT-BUFFER is a buffer or string name of a buffer.
OPT-LIST is a list of strings which are additional options to gphelp.

Any ANSI escapes for bold, underlining, colours, etc, in
OUTPUT-BUFFER are crunched to Emacs faces using ansi-color.el.
Those escapes come from gphelp -detex.  The crunching is harmless
for other output."

  ;; This function is also used at the very beginning through gp-cpl-init to
  ;; get all the function names for the completion mechanism.

  ;; `default-directory' might not exist in a new file buffer.  In that case
  ;; run from the root directory since `call-process' must have a current
  ;; directory for the subprocess.  If default-directory does exist then
  ;; leave it alone since it might be a good place for "xdvi" to save or
  ;; print to file (though gphelp circa version 2.7 switches to a tempdir).
  ;;
  ;; `coding-system-for-read' is `utf-8' for gphelp -utf8 option and this is
  ;; also ok for plain ascii without -utf8.
  ;;
  (with-current-buffer output-buffer
    (let ((default-directory (if (file-directory-p default-directory)
                                 default-directory "/"))
          (gphelp (if gp-gphelp-dir
                      (expand-file-name "gphelp" gp-gphelp-dir)
                    "gphelp"))
          (coding-system-for-read 'utf-8)
          (process-connection-type nil) ;; pipe rather than pseudo-tty
          (process-environment (copy-sequence process-environment)))
      (setenv "COLUMNS" (number-to-string win-size))

      ;; Detect whether "gphelp -utf8" can be used.
      ;; gphelp before 2.8.x exits code 1 for option -utf8 unknown.
      ;; Ignore `file-error' for program not found in this detect
      ;; (it is trapped and displayed in the run below).
      (when (eq 'detect gp-gphelp-coding)
        (ignore-errors
          (setq gp-gphelp-coding
                (if (and (char-displayable-p (decode-char 'ucs 233)) ;; e-acute
                         (equal 0 (call-process gphelp
                                                nil  ;; stdin
                                                nil  ;; stdout+stderr discard
                                                nil  ;; redisplay during output
                                                "-utf8" "-detex" "gcd")))
                    'utf-8 'ascii))))

      (condition-case err
          (apply 'call-process
                 gphelp
                 nil  ;; stdin
                 t    ;; stdout+stderr current buffer
                 t    ;; redisplay during output
                 (append (and (eq gp-gphelp-coding 'utf-8) '("-utf8"))
                         opt-list
                         (list word)))
        (file-error
         ;; if gphelp program not found then show Emacs error message
         (insert (error-message-string err))))

      ;; Convert ANSI escapes like \033[4m from gphelp -detex.
      ;; ansi-color.el defaults to applying overlays, and they accumulate
      ;; endlessly even when erase-buffer, so explicit remove-overlays here.
      (remove-overlays)
      (require 'ansi-color)
      (ansi-color-apply-on-region (point-min) (point-max)))))

(defun gp-replace (a b)
  "Replace the regexp a by the string b everywhere in the current buffer"
  ;; b may be an expression whose value is a string, like
  ;; (buffer-substring-no-properties (match-beginning 0) (match-end 0))
  (save-excursion
   (goto-char (point-min))
   (while (re-search-forward a nil t)
     (replace-match (eval b) t t))))

(defconst gp-word-or-operator-at-point-regexp 
  ;; operators = == === ! != + ++ +=
  ;;           - -- -= ->       but -> not known to gphelp
  ;;           * *= / /= /*     but /* not known to gphelp and ?/* hangs
  ;;           % %=
  ;;           < << <= <<=  > >> >= >>=
  ;;           && || 
  ;;           \ \/ \\ \=
  ;;           ^  but not ^=
  ;;           . ' ~
  ;; not parens () prefer the word before the paren
  ;; not braces {} as { hangs presently
  "[a-zA-Z_][a-zA-Z0-9_]*\
\\|=\\{1,3\\}\
\\|!=?\
\\|\\+[+=]?\
\\|-[-=>]?\
\\|[*%]=?\
\\|/[=*]?\
\\|<<?=?\
\\|>>?=?\
\\|&&\\|||\
\\|\\\\[/\\=]?\
\\|[^ \t\r\n0-9a-zA-Z(){}]"
  "An internal part of gp-help.el.
A regexp matching a word or operator.  A word is alphanumeric.
An operator is a single non-alphanumeric character and the
various multi-char forms known to gp such as <<= \/ etc.

Only known multi-char operators are matched, so that operators
without whitespace between them are not run together.")

(defun gp-word-or-operator-at-point ()
  "Return a word or operator name at point, or nil if none.
This is designed to get a function or operator suitable to pass
to \"gphelp\" or the gp \"?\" command.

Point can be at the start, end or anywhere in the middle of a
word or operator.  If point is exactly between two candidates
then the word or operator after is returned."

  ;; Look at non-whitespace at point and search match of
  ;; `gp-word-or-operator-at-point-regexp' which is furthest back and
  ;; extends up to or past point.  The various gp operators matched mean a
  ;; simple word boundary is not enough.
  (let ((orig-point (point))
        ret)
    (save-excursion
      (skip-syntax-backward "^-") ;; to start of non-whitespace
      (while (and (not ret)
                  (not (eobp))
                  (<= (point) orig-point))
        (when (looking-at gp-word-or-operator-at-point-regexp)
          (if (>= (match-end 0) orig-point)
              (setq ret (match-string 0)))
          (when (= (match-end 0) orig-point)
            ;; Match ends at point.  But prefer a match after point if there
            ;; is both before and after.
            (goto-char orig-point)
            (if (looking-at gp-word-or-operator-at-point-regexp)
                (setq ret (match-string 0)))))
        (goto-char (1+ (point)))))
    ret))

(defun gp-ask-name-wisely (this-type)
  "Ask in the minibuffer for a THIS-TYPE name and provide a default.
THIS-TYPE is a human-readable string telling the user what to
enter, eg. \"Function\".
Completions are from `gp-c-array'.
Operator names such as == can be entered too."
  (let* ((default (gp-word-or-operator-at-point))
         (keymap  (make-sparse-keymap)))
    (set-keymap-parent keymap minibuffer-local-completion-map)
    ;; "space" is usually 'minibuffer-complete-word, but C-i does that.
    ;; Space can usefully be entered for `gp-get-apropos'.
    (define-key keymap " " 'self-insert-command)
    (let* ((minibuffer-local-completion-map keymap)
           (arg (completing-read (if default
                                     (format "%s (%s): " this-type default)
                                   (format "%s: " this-type))
                                 gp-c-array  ;; completion collection
                                 nil         ;; predicate
                                 nil         ;; require-match
                                 nil         ;; initial-input
                                 nil         ;; history
                                 default)))
      ;; Do not use "" as fn since it does not produce a help window, use
      ;; " " instead.
       (if (equal arg "")
          '(" ")
        (list arg)))))

(defun gp-get-TeX-man-entry (fn)
  "Run gphelp to display the TeX help on function FN.
Similar to '??fn' under GP."
  (interactive (gp-ask-name-wisely (gp-messager 21)))
  (with-temp-buffer
    (gp-call-gphelp-olist (window-width) fn (current-buffer) nil)
    (goto-char (point-min))
    (cond ((looking-at "\n\\|\\'")  ;; no output, success
           )
          ((search-forward " not found !" nil t)
           (message (gp-messager 20) fn))  ;; unknown function
          (t
           ;; some other output, show it to the user
           (message "%s" (buffer-string))))))

(defun gp-get-man-entry (fn)
  "Get the description of fn from chapter 3 of the manual
via gphelp, and display the result in a new window.
If there is no entry for fn in the manual, send ?fn to gp.
If a definition is found, add fn to the array of possible completions"

  (interactive (gp-ask-name-wisely "Function"))
  (let ((wind (selected-window)))
    ;; We switch to the buffer *gp-help* and erase its content:
    (set-buffer (get-buffer-create "*gp-help*"))
    (erase-buffer)
    (gp-call-gphelp-olist (window-width (get-buffer-window "*gp-help*"))
                            fn (current-buffer) '("-detex"))

    (goto-char (point-min))
    (if (save-excursion
           (goto-char (- (point-max) 13))
           (looking-at " not found !"))
           ;; If gp was not running then start it.
           (if (gp-background)
             (progn 
               (gp-meta-cmd-general (concat "?" fn) nil)
               ;; which sets the buffer "*gp-help*".
               ;; GP circa 2.7 prints "foo: unknown identifier"
               ;; Past GP prints      "*** unknown identifier"
               ;; and similarly for "user defined variable"
               (if (looking-at "\\( *\\*\\*\\*\\|.*?:\\) *unknown identifier")
                 (progn 
                   (gp-window-manager "*gp-help*" 'gp-remove-help-now)
                   (message (gp-messager 20) fn))
		 (if (looking-at "\\( *\\*\\*\\*\\|.*?:\\) *user defined variable")
                 (progn
                   (gp-window-manager "*gp-help*" 'gp-remove-help-now)
                   (message (gp-messager 22) fn))
               ;; Else tell user how to remove the help window:
               (gp-window-manager "*gp-help*" 'gp-show-help)
               (gp-info-wind-conf)
               ;; and let the completion system know about the function name:
               (gp-add-symbol fn)))))
         ;; Else show the help buffer and tell user how to remove help window:
         (gp-window-manager "*gp-help*" 'gp-show-help)
         (gp-info-wind-conf))
    (set-buffer-modified-p nil)
    (select-window wind)))  ; End of 'gp-get-man-entry

(defun gp-buffer-to-double-list nil
  (let ((lst nil))
    (save-excursion
      (while (not (eobp))
         (add-to-list 'lst
                (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))
         (forward-line 1)))
    (delete-region (point) (point-max))
    (gp-split-menu lst)))

(defun gp-get-apropos (exp)
"Show in gp-menu-mode the functions or sections
in whose description the expression EXP appears.
Similar to \"??? exp\" in gp."
  (interactive (gp-ask-name-wisely (gp-messager 32)))

  (gp-window-manager "*gp-menu*" 'gp-beginning)
  (insert (format (concat "\n" (gp-messager 31) "\n") exp))
  (insert "\n###\n")  ; To give this buffer the format of a gp-menu file.
  (gp-call-gphelp-olist 50 exp (current-buffer) '("-k" "-raw"))
  (gp-replace "\\\\_" "_")

  (search-backward "\n###\n")
  (forward-char 5)
  (if (eobp)
      (progn
        (kill-buffer "*gp-menu*")
        (gp-backward-wind-conf)
        (message (gp-messager 23) exp))
      (set-buffer "*gp-menu*")
      (let ((adoublelist (gp-buffer-to-double-list)))
        (gp-display-simple-menu (car adoublelist))
        (goto-char (point-max))
        (and (car adoublelist) (insert "\n"))
        (gp-display-special-menu (nth 1 adoublelist)))
      (setq gp-menu-start-keywords 0 gp-menu-end-keywords 0)
      (setq major-mode 'gp-menu mode-name "GP MENU")
      (use-local-map gp-menu-map)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (search-forward "\n###\n")
      (gp-menu-info)))

;;------------
;; TeX MANUAL
;;------------

;; The line ";;;###autoload" is useless.
;; It will be useful when pari.el will be part
;; of the usual distribution of emacs.
;;;###autoload
(defun gpman()
  "Start up xdvi with the gp manual."

  (interactive)
;; Run gp-mode-hook in case it specifies a different version of the manual.
  (run-hooks 'pari-mode-hook 'gp-mode-hook)
  (gp-get-TeX-man-entry ""))

(defun gp-tutorial()
  "Start up xdvi with the gp tutorial."

  (interactive)
;; Run gp-mode-hook in case it specifies a different version of the tutorial.
  (run-hooks 'pari-mode-hook 'gp-mode-hook)
  (gp-get-TeX-man-entry "tutorial"))

;;-------------------
;; Help on gp-*-mode
;;-------------------

(defun gp-show-pariemacs nil
  "Show pariemacs.txt on another window."
  (interactive)
;; Run gp-mode-hook in case it specifies a different version of README dile,
;; which was earlier called pariemacs.txt.
  (run-hooks 'pari-mode-hook 'gp-mode-hook)
;; On a well-configured system, the variable gp-pariemacs contains the name of the
;; proper file. But since users requiring this file may precisely be the ones
;; who do not know how to configure stuff properly ...
  (let ((wind (selected-window))
        (where-it-is "")
        (to-be-tested (list "/usr/local/lib/pari/emacs/"
                            "/usr/local/lib/pari/"
                            "/usr/local/share/lib/pari/"
                            "/usr/local/share/lib/pari/emacs/"
                            "/usr/share/lib/pari/"
                            "/usr/share/lib/pari/emacs/"
                            (concat "/usr/local/lib/pari-" gp-version "/")
                            (concat "/usr/local/lib/pari-" gp-version "/emacs/")
                            (concat "/usr/local/share/lib/pari-" gp-version "/")
                            (concat "/usr/local/share/lib/pari-" gp-version "/emacs/")
                            (concat "/usr/share/lib/pari-" gp-version "/emacs/")
                            (concat "/usr/share/lib/pari-" gp-version "/"))))

    ;; The directory we loaded from should have the exact corresponding
    ;; README.  Make this the first choice for the search.
    (let ((filename (symbol-file 'gp-show-pariemacs)))
      (if filename ;; can be nil if we exist due to `eval' rather than `load'
          (setq to-be-tested (cons (file-name-directory filename)
                                   to-be-tested))))

    ;; Locate pariemacs.txt = (new-name) README:
    (if (file-exists-p gp-pariemacs)
        (progn (setq where-it-is gp-pariemacs))
      (mapcar (lambda (afile) 
                (if (and (file-exists-p afile)
                         ; if it already exist, stop!
                         (not (file-exists-p where-it-is))) 
                    (setq where-it-is afile)))
              (mapcar (lambda (apath) (expand-file-name (concat apath "/README")))
                      (append to-be-tested load-path))))

    (if (not (string-equal where-it-is ""))
      (progn
        ;; We switch to the buffer *gp-help* and erase its content:
        (set-buffer (get-buffer-create "*gp-help*"))
        (setq buffer-read-only nil)
        (erase-buffer)
        (message where-it-is)  ;; tell *Messages* which version of pariemacs is used.
        (insert-file where-it-is)
        ;; Show the help buffer and tell user how to remove help window:
        (gp-window-manager "*gp-help*" 'gp-show-help)
        (setq buffer-read-only t)
        (search-forward "Common commands" nil t)
        (beginning-of-line) (set-window-start (selected-window) (point))
        (gp-info-wind-conf)
        (select-window wind))
      ;; Tell the user the file was not found:
      (gp-show-help (gp-messager 39))))
  )  ; End of 'gp-show-pariemacs

;;----------------- Outsiders :

(defun gp-menu-quit nil
  "Switch the *PARI* buffer if it exists, or (other-buffer) if it does not."
  (interactive)
  (gp-window-manager "*gp-menu*" 'gp-remove-help-now)
  (if (get-buffer-window "*gp-help*")
      (progn (gp-info-wind-conf)
             (if (string= (buffer-name) "*gp-help*")
                 (select-window (other-window 1))))))

(defun gp-cpl-init nil
"Add all the commands listed by gphelp -k \"\" to the obarray
used for completion."
  (with-temp-buffer
    (gp-call-gphelp-olist 100 " " (current-buffer) '("-k" "-raw"))
    ;; gphelp circa 2.7.4 backslashes underscores, eg.  "factor\_proven".
    ;; Want plain name here.
    (gp-replace "\\\\_" "_")
    (goto-char (point-min))
    (let ((adoublelist (gp-buffer-to-double-list)))
      (mapcar 'gp-add-symbol (car adoublelist)))))

(defconst gp-help-menu
   (list
   (list "Info ..."
         (vector (gp-messager 94) 'gp-get-apropos
                 :label '(gp-messager 94))
         (vector (gp-messager 95) 'gp-get-man-entry
                 :label '(gp-messager 95)))
   (list "TeX Info"
         (vector (gp-messager 96) 'gpman ':active t ':key-sequence nil
                 :label '(gp-messager 96))
         (vector (gp-messager 97) 'gp-tutorial ':active t ':key-sequence nil
                 :label '(gp-messager 97))
         (vector (gp-messager 95) 'gp-get-TeX-man-entry ':active t ':key-sequence nil
                 :label '(gp-messager 95)))
    ;; "Browsing through predefined functions"
   (vector (gp-messager 93) 'gp-browser ':active t 
           :label '(gp-messager 93)
           ':included (eq window-system 'x) ':key-sequence nil)
   (vector (gp-messager 63) 'gp-show-pariemacs
           :label '(gp-messager 63))))

(add-hook 'pari-mode-hook
  '(lambda nil 
     (define-key gp-map "\M-?"          (function gp-get-man-entry))
     (define-key gp-map "\M-H"          (function gp-get-apropos))
     (define-key gp-script-map "\M-?"   (function gp-get-man-entry))
     (define-key gp-script-map "\M-H"   (function gp-get-apropos))
     (unless gp-c-array-createdp
       (gp-cpl-init)
       (setq gp-c-array-createdp t))))

(add-hook 'pari-menu-bar-update-hook
  '(lambda nil ;; The item (gp-messager 48) should already exist.
               ;; It is build at the very beginning.
     (when (and gp-menu-barp 
                (or (and (eq major-mode 'gp-mode)
                         (= gp-menu-map-level 1))
                    (and (eq major-mode 'gp-script-mode)
                         (= gp-script-menu-map-level 1))))
       (mapcar
        (lambda (item)
          (if (listp item)
              (progn
                (when (eq major-mode 'gp-mode)
                  (easy-menu-change '("GP") (car item) (cdr item) (gp-messager 48)))
                (when (eq major-mode 'gp-script-mode)
                  (easy-menu-change '("GP-script") (car item) (cdr item) (gp-messager 48))))
            (when (eq major-mode 'gp-script-mode)
              (easy-menu-add-item GP-script-menu-map nil item (gp-messager 48)))
            (when (eq major-mode 'gp-mode)
              (easy-menu-add-item GP-menu-map nil item (gp-messager 48)))
            ))
        gp-help-menu)
       (when (eq major-mode 'gp-mode)
         (easy-menu-change '("GP") "--" nil (gp-messager 48)))
       (when (eq major-mode 'gp-script-mode)
         (easy-menu-change '("GP-script") "--" nil (gp-messager 48)))
       (when (eq major-mode 'gp-mode)
         (message "Menu bar item GP: help part loaded."))
       (when (eq major-mode 'gp-script-mode)
         (message "Menu bar item GP-script: help part loaded.")))))

;; pari-help.el ends here.
