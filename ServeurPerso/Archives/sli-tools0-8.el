;; sli-tools.el --- structured languages indentation package

;; It works out some tools for indentation of structured programs.
;; It has been written for mupad.el and pari.el but should apply to
;; any other structured language like Pascal.
;; See sli-tools and sli-structures below.

;; Maintainer: Olivier Ramare <ramare@agat.univ-lille1.fr>

;; version 0.8

;; BUGS:
;; If I remember well, strings spreading over several lines may
;; raise some troubles.

(provide 'sli-tools) 

;;------------------------------------------------------
;; Variables that defines how indentation should occur.
;; See mupad.el for an example.
;;------------------------------------------------------

;; We use "" and  \" for strings.

(defvar sli-structures nil
  "List of lists. Each item is a vector or a list which we call a STRUCTURE
in this explanation. There are several kind of structures :

([HEAD-STRING head INDENT-HEAD]
 [SOFT-STRING1 soft INDENT-SOFT1]
 ([STRONG-STRING1 strong INDENT-STRONG1]
  [SOFT-STRING2 soft INDENT-SOFT2])
 ([STRONG-STRING2 strong INDENT-STRONG2])
 [END-STRING end])
is the usual structure, like in 'if/then/(elif/then)/(else)/end_if'.
Between the 'head' and the 'soft', INDENT-HEAD is used on subsequent lines
to offset the new line with respect to the beginning of HEAD-STRING. When
the 'soft' is found, INDENT-SOFT1 is used still with respect to the 'head'.
The next part is optional.
The STRONG-STRING is aligned on its 'head' and INDENT-STRONG is used after
that, with respect to the STRONG-STRING. Finally the END-STRING is aligned
on the previous STRONG-STRING (the 'heredity principle'). If you want to
change this alignement, use `sli-shift-alist' below.
In this construct, you can also use
[SPECIAL-HEAD-STRING special-head INDENT-SPECIAL-HEAD SEPARATOR]
This key is closed by SEPARATOR which belongs to `sli-separators'
and no other construct in between (except comments); for instance the
'proc/(option)/begin/end_proc' construct of MuPAD is a head/special-head/strong/end.
You can use several [END-STRING end]. The first one is going
to be used by the maid. Furthermore you can use the same END-STR for
several constructs. It then applies to the first 'head' that appears
(going backward).

([BEACON-STRING beacon INDENT-BEACON])
specifies a special string that can be found between a 'head' or a 'strong'
and its corresponding 'soft'. The typical example being 'for t from 1 to 2 do'
and has pattern 'head/beacon/beacon/soft'. If a newline is asked after the
'from' but before the 'to', indentation is done with respect to the beginning
of 'from' and INDENT-BEACON is added. 'math-relation's below are beacons.

([RELATION-STRING math-relation INDENT-RELATION])
specifies a mathematical type of relation (like '='). Such operators acts either
as beacons  (example 'while t=55 do' with pattern 'strong/math-relation/soft')
or else are closed by someone in `sli-separators'. They may contain further
structures in between like in 'foo = if ok then gonethrough=t ; 3 else 5 end_if'.
INDENT-RELATION is used before the appearance of the proper separator.

HEAD-STRING's, MATH-RELATION-STRING's, BEACON-STRING's,
SEPARATOR's should all be different.
SOFT-KEY's and STRONG-KEY's are different from any of the above, but a same soft
key can be used in different constructs. Usual examples are 'then' and 'do'.
But because of the way things are, the corresponding INDENT should be
the same throughout.

Cdr's are to be evaled.

Technical note: the first element of this list *has to* contain a 'head'. ")

(defvar sli-escape-key-string ""
"The strings used as separators, relations, and all")

(defvar sli-shift-alist nil
"Usual 'strong/end' are aligned on the previous
occurence of a corresponding head/strong.
You can add an offset between two keys.
Element of this list have format ([key1 key2] . offset).
Cdr's are to be evaled.")

(defvar sli-no-heredity-list nil
"Usual 'strong/end' are aligned on the previous
occurence of a corresponding head/strong except
if memtionned in this list.
Elements of this list have format [head-key key].")

(defvar sli-separators nil "Do not forget `sli-is-a-separatorp'.")

(defvar sli-is-a-separatorp-fn 'sli-is-a-separatorp-default)

(defun sli-is-a-separatorp-default (&optional pt)
  (looking-at (regexp-opt sli-separators)))

(defun sli-is-a-separatorp (&optional pt)
  (funcall sli-is-a-separatorp-fn pt))

(defvar sli-safe-place-regexp "^\\(//--+\\|/\\*-+-\\*/\\)$"
"Marker used to tell emacs this point is outside a commented area, a string or a sexp.")

(defvar sli-fixed-keys-alist '()
"Some keys should be placed at a fixed place. This is the corresponding alist.
List of (STRING . INDENTATION).")

(defvar sli-keys-with-newline nil
"When `sli-maid' tries to further your constructs, some keys should be
followed by a newline before the completion is added.")

(defvar sli-add-to-key-alist nil "See `sli-maid'.")

(defvar sli-more-maidp t "See `sli-maid'.")

(defvar sli-tab-always-indent t "See `sli-electric-tab'.")

(defvar sli-comment-starts '()
"A list of possible starters of one-line comments.
That is to say an extension of `comment-start' in this special case.")
;;;--------------------------------------------------------------------------
;;; Inner variables
;;;--------------------------------------------------------------------------

(defvar sli-head-keys nil)
(defvar sli-special-head-keys nil)
(defvar sli-soft-keys nil)
(defvar sli-beacon-keys nil)
(defvar sli-math-relation-keys nil)
(defvar sli-relation-keys nil)
(defvar sli-keys-nomrelations nil) ; nomrelations means no-math-relations
(defvar sli-strong-keys nil)
(defvar sli-end-keys nil)
(defvar sli-keys nil)
(defvar sli-max-keys-length 0
"An integer: the maximum length of a keyword in sli-structures.")

(defvar sli-all-keys-nomrelations-regexp nil)
(defvar sli-all-keys-regexp nil) ; including string quotes and all kind of comments.
(defvar sli-all-end-strong-regexp nil)
(defvar sli-fixed-regexp nil)

(defvar sli-head-end-alist nil "The alist ((end . head) ...).")
(defvar sli-ends-head-alist nil "The alist ((head . (end1 end2 ...) ...).")
(defvar sli-heads-strong-alist nil "The alist ((strong . (head1 head2 ...)) ...).")
(defvar sli-special-head-alist nil "The alist ((special-head . separator) ...).")
(defvar sli-companion-strong-keys-alist nil
"The alist  ((strong/head . (strongs that could be after)) ...).
The car should be a member of the cdr if the car is a strong.")
(defvar sli-soft-alist nil "The alist ((ambiguous-soft . (head-or-strong1 head-or-strong2 ...)) ...).")
(defvar sli-soft-head-or-strong-alist nil "The alist ((head-or-strong . soft) ...)")
(defvar sli-first-offset-alist nil)  ; to apply before the soft
        ; it applies to head/strong keys that are followed by a soft with no
        ; head or strong in between. Morally speaking this soft "closes" the head/strong.
(defvar sli-relevant-alist nil
"An alist. Put all head/strong/end's in one bundle. say two keys are linked if
they occur in a same constructs. Close this relation transitively.
this is the alist ((key . (keys in the same class)) ...).")
(defvar sli-ancestors-alist nil)

(defvar sli-second-offset-alist nil )  ; to apply after the soft
(defvar sli-relation-offset-alist nil)

(defvar sli-maid-alist nil)
(defvar sli-ambiguous-keys nil
"List of keys that may ask for a different
following key according to context.
They *should be* soft or strong keys.")

(mapcar 'make-variable-buffer-local
 '(sli-structures sli-shift-alist sli-separators sli-is-a-separatorp-fn
   sli-more-maidp sli-add-to-key-alist sli-math-relation-keys
   sli-max-keys-length sli-no-heredity-list
   sli-head-keys sli-special-head-keys sli-soft-keys sli-beacon-keys
   sli-relation-keys sli-keys-nomrelations sli-strong-keys sli-end-keys sli-keys
   sli-all-keys-nomrelations-regexp sli-all-keys-regexp sli-all-end-strong-regexp
   sli-soft-head-or-strong-alist sli-head-end-alist sli-heads-strong-alist
   sli-special-head-alist sli-ends-head-alist sli-relevant-alist
   sli-ancestors-alist sli-fixed-keys-alist sli-fixed-regexp
   sli-companion-strong-keys-alist sli-soft-alist sli-first-offset-alist
   sli-second-offset-alist sli-relation-offset-alist sli-shift-alist
   sli-maid-alist sli-ambiguous-keys))

;;;-----------------------------------------------------------------------------
;;; This section is devoted to some precomputations from sli-structures.
;;; Lots of work is done several time, but I prefer this modularity
;;; since it is easier to modify.  
;;;-----------------------------------------------------------------------------

(defun sli-split-list (lst)
  (let ((wordother '()) (otherword '()) (wordword '()) (otherother '()) ls)
    (mapcar
     (lambda (wd)
       (setq ls (string-to-list wd))
      (cond
       ((and (= (char-syntax (car ls)) ?w) (= (char-syntax (car (last ls))) ?w))
        (add-to-list 'wordword wd))
       ((= (char-syntax (car ls)) ?w)
        (add-to-list 'wordother wd))
       ((= (char-syntax (car (last ls))) ?w)
        (add-to-list 'otherword wd))
       (t (add-to-list 'otherother wd))))
     lst)
    (list wordword wordother otherword otherother)))

(defun sli-regexp-opt (lst)
  (let ((qlst (sli-split-list lst)))
    (if (null (elt qlst 0))
        (if (null (elt qlst 1))
            (if (null (elt qlst 2))
                (if (null (elt qlst 3))
                    "\\<\\>"
                  (regexp-opt (elt qlst 3) t)) ; grouping required for posix
              (concat
               (regexp-opt (elt qlst 2) t) "\\>"
               (if (null (elt qlst 3))
                   ""
                 (concat "\\|" (regexp-opt (elt qlst 3) t)))))
          (concat
            "\\<" (regexp-opt (elt qlst 1) t)
            (if (null (elt qlst 2))
                (if (null (elt qlst 3))
                    ""
                  (concat "\\|" (regexp-opt (elt qlst 3) t)))
              (concat
               "\\|" (regexp-opt (elt qlst 2) t) "\\>"
               (if (null (elt qlst 3))
                   ""
                 (concat "\\|" (regexp-opt (elt qlst 3) t)))))))
      (concat
       "\\<" (regexp-opt (elt qlst 0) t) "\\>"
       (if (null (elt qlst 1))
            (if (null (elt qlst 2))
                (if (null (elt qlst 3))
                    ""
                  (concat "\\|" (regexp-opt (elt qlst 3) t)))
              (concat
               "\\|" (regexp-opt (elt qlst 2) t) "\\>"
               (if (null (elt qlst 3))
                   ""
                 (concat "\\|" (regexp-opt (elt qlst 3) t)))))
          (concat
            "\\|\\<" (regexp-opt (elt qlst 1) t)
            (if (null (elt qlst 2))
                (if (null (elt qlst 3))
                    ""
                  (concat "\\|" (regexp-opt (elt qlst 3) t)))
              (concat
               "\\|" (regexp-opt (elt qlst 2) t) "\\>"
               (if (null (elt qlst 3))
                   ""
                 (concat "\\|" (regexp-opt (elt qlst 3) t)))))))))))

(defun sli-flatten (ls)
  (let ((res '()))
    (mapcar
      (lambda (ph)
        (cond
          ((listp ph) (setq res (append res (sli-flatten ph))))
          (t (setq res (append res (list ph))))))
      ls)
    res))

(defun sli-scan-structures-locally (stru symbol)
  (let ((res '()))
    (mapcar (lambda (ph)
              (setq res
                (append res
                  (cond
                    ((listp ph) (sli-scan-structures-locally ph symbol))
                    ((equal (elt ph 1) symbol) (list (elt ph 0)))
                    (t '())))))
            stru)
    res))

(defsubst sli-compact-list (lst)
  ; remove same consecutive occurences.
  (let* ((old (car lst)) (nlst (list old))  (lst (cdr lst)))
    (while lst
      (if (equal (car lst) old)
          (setq lst (cdr lst))
          (setq nlst (cons (setq old (car lst)) nlst) lst (cdr lst))))
    (nreverse nlst)))

(defun sli-scan-structures (symbol)
  (let ((res '()))
    (mapcar
      (lambda (st)
        (when (equal (elt st 1) symbol)
          (add-to-list 'res (elt st 0))))
      (sli-flatten sli-structures))
  res))

(defun sli-get-ends-head-alist nil
  (let ((res '()) all-ends) ; forme la liste (head-key . (end1 end2 ...))
   (mapcar
     (lambda (ph)
       (when (equal (elt (elt ph 0) 1) 'head)
         (setq all-ends '())
         (mapcar
           (lambda (s)
             (when (and (vectorp s) (equal (elt s 1) 'end))
               (setq all-ends (append all-ends (list (elt s 0))))))
          ph)
         (add-to-list 'res (cons (elt (elt ph 0) 0) all-ends))))
     sli-structures)
   res))

(defun sli-get-head-end-alist nil
  (let ((res '()) all-heads) ; forme la liste (end-key . (head1 head2 ...))
   (mapcar
     (lambda (end)
       (setq all-heads '())
       (mapcar
	(lambda (s)
	  (if (member end (cdr s))
	      (add-to-list 'all-heads (car s))))
	sli-ends-head-alist)
       (add-to-list 'res (cons end all-heads)))
   sli-end-keys)
   res))

(defun sli-get-strong (ph)
  (let ((res '()))
    (mapcar
      (lambda (st)
        (when (equal (elt st 1) 'strong)
          (add-to-list 'res (elt st 0))))
      ph)
  res))

(defun sli-get-heads-strong-alist nil
  (let ((res '()) (aux '()) possible-heads) ; forme la liste des (strong-key . (head-key1 head-key2 ...))
   ; Peut-etre plusieurs strong pour chaque head.
   (mapcar
     (lambda (ph)
       (if (equal (elt (elt ph 0) 1) 'head)
           (let ((strongs (sli-get-strong (sli-flatten ph))))
              (unless (null strongs)
                 (mapcar (lambda (st)
                            (setq aux (add-to-list 'aux
                                  (cons st (elt (elt ph 0) 0)))))
                         strongs)))))
     sli-structures)
   ; Une strong peut etre liee a plusieurs heads. Il faut les reunir:
   (mapcar
    (lambda (strong)
      (setq possible-heads '())
      (mapcar
       (lambda (ajoint)
         (when (equal (car ajoint) strong)
           (setq possible-heads (append possible-heads (list (cdr ajoint))))))
       aux)
      (when (> (length possible-heads) 1)
        (add-to-list 'sli-ambiguous-keys strong))
      (setq res (append res (list (cons strong possible-heads)))))
    (sli-compact-list (sort (mapcar 'car aux) 'string-lessp)))
   res))

(defun sli-get-soft-alist nil ; forme la liste (soft . (head ot strong using it))
  (let ((resaux '()) loc (res '()) astrong-list (asoft-list '()))
   (mapcar
    (lambda (ph)
      (setq astrong-list '())
      (mapcar
       (lambda (ve)
	 (cond
          ((equal (elt ve 1) 'soft) (unless (null astrong-list)
                                      (add-to-list 'resaux (cons (elt ve 0) astrong-list))
                                      (add-to-list 'asoft-list (elt ve 0))))
          ((member (elt ve 1) '(strong head)) (setq astrong-list (list (elt ve 0))))))
       (sli-flatten ph)))
    sli-structures)
    ;; now gather identical soft:
    (mapcar
      (lambda (asoft)
        (setq loc '())
        (mapcar
	  (lambda (dd)
	    (when (string-equal asoft (car dd))
              (setq loc (append loc (cdr dd)))))
          resaux)
        (add-to-list 'res (cons asoft (sli-compact-list (sort loc 'string-lessp)))))
      asoft-list)
    res
   ))

(defun sli-common-pointp (l1 l2)
  (let ((ok nil))
    (mapcar (lambda (c) (setq ok (or ok (member c l1)))) l2)
    ok))

(defun sli-get-companion-alist nil ; case ?? It was not there.
  (let ((res '()))
    ; on prend les car de sli-heads-strong-alist on leur
    ; associe la liste des car qui ont au moins une tete en commun :
    (mapcar
      (lambda (co)
        (let ((end (cdr co)) (companions '()))
          (mapcar
            (lambda (coo)
               (when (sli-common-pointp (cdr coo) end)
                 (setq companions (add-to-list 'companions (car coo)))))
            sli-heads-strong-alist)
          (setq res (append res (list (cons (car co) companions))))))
      sli-heads-strong-alist)
    ; on prend les cdr de sli-heads-strong-alist on leur
    ; associe la liste des car possibles :
    (mapcar
      (lambda (head)
        (let ((companions '()))
          (mapcar
            (lambda (coo)
               (when (member head (cdr coo))
                 (setq companions (add-to-list 'companions (car coo)))))
            sli-heads-strong-alist)
          (setq res (add-to-list 'res (cons head companions)))))
      (sli-compact-list (sort (sli-flatten (mapcar 'cdr sli-heads-strong-alist)) 'string-lessp)))
    res))

(defun sli-get-soft-head-or-strong-alist nil
  (let ((res '()) asoft astrong-list)
    (mapcar
     (lambda (ass)
       (setq asoft (car ass))
       (setq res (append res (mapcar (lambda (st) (cons st asoft)) (cdr ass)))))
     sli-soft-alist)
    res))

(defun sli-equivalence-classes-local (lst)
  (cond
   ((null lst) lst)
   (t (let (lstbis (done nil) (l1 (car lst)))
	(setq lstbis
	      (mapcar
	       (lambda (c)
		 (if (sli-common-pointp l1 c)
		     (progn
		       (setq done t)
		       (sli-compact-list (sort (append l1 c) 'string-lessp)))
		   c))
	       (sli-equivalence-classes-local (cdr lst))))
	(unless done
	  (setq lstbis (append lstbis (list l1))))
	lstbis))))

(defun sli-equivalence-classes (lst)
  (while (> (length lst) (length (setq lst (sli-equivalence-classes-local lst)))))
  lst)

(defun sli-get-relevant-alist nil
  (let (key-lst (res '()))
    ;; relevant keys are head/strong or end keys.
    (mapcar
     (lambda (class)
       (mapcar
	(lambda (el)
	  (add-to-list 'res (cons el class)))
	class))
     (sli-equivalence-classes
      (delq nil ; nil had better not be the first one ...
	    (mapcar
	     (lambda (ph)
	       (setq key-lst '())
	       (mapcar
		(lambda (co)
		  (when (member (elt co 1) '(head strong end))
		    (add-to-list 'key-lst (elt co 0))))
		ph)
	       key-lst)
	     (mapcar 'sli-flatten sli-structures)))))
    res))

(defun sli-get-ancestors-alist nil
  (append
   (mapcar
    (lambda (end)
      (cons end
	    (sli-flatten
	     (mapcar
	      (lambda (head)
		(or (assoc head sli-companion-strong-keys-alist) ; works only if a strong is present
		    (cdr (assoc end sli-head-end-alist))))
	      (cdr (assoc end sli-head-end-alist))))))
    sli-end-keys)
   (mapcar
    (lambda (strong)
      (cons strong
            (append (cdr (assoc strong sli-heads-strong-alist))
                    (cdr (assoc strong sli-companion-strong-keys-alist)))))
    sli-strong-keys)))

(defun sli-get-first-offset-alist nil
  (let ((res '()) last-head-or-strong stru pl)
    (mapcar
     (lambda (ph)
       (setq last-head-or-strong nil stru (sli-flatten ph))
       (while (not (null stru))
         (setq pl (car stru))
         (cond
           ((member (elt pl 1) '(head strong)) (setq last-head-or-strong pl))
           ((equal (elt pl 1) 'soft)
            (when last-head-or-strong
              (setq res (append res (list (cons (elt last-head-or-strong 0)
                                                (elt last-head-or-strong 2))))
                    last-head-or-strong nil))))
           (setq stru (cdr stru))))
     sli-structures)
    res))

(defun sli-get-second-offset-alist nil
  (let ((res '()) last-cand stru pl)
    (mapcar
     (lambda (ph)
       (setq last-cand nil stru (sli-flatten ph))
       (while (not (null stru))
         (setq pl (car stru))
         (cond
           ((equal (elt pl 1) 'head)
            (setq last-cand pl))
           ((member (elt pl 1) '(end strong special-head))
            (when last-cand ;; no soft after last-cand.
              (setq res (append res (list (cons (elt last-cand 0)
                                                (elt last-cand 2))))))
            (if (equal (elt pl 1) 'end)
                (setq last-cand nil)
              (setq last-cand pl)))
           ((equal (elt pl 1) 'soft)
            (when last-cand ;; last-cand is followed by a soft
              (setq res (append res (list (cons (elt last-cand 0)
                                                (elt pl 2))))
                    last-cand nil))))
	 (setq stru (cdr stru))))
     sli-structures)
    res))

(defun sli-get-relation-offset-alist nil
  (let ((res '()) pl)
    (mapcar
      (lambda (ph)
        (cond
          ((member (elt (setq pl (elt ph 0)) 1) '(math-relation beacon))
           (add-to-list 'res (cons (elt pl 0) (elt pl 2))))))
      sli-structures)
    res))

(defun sli-get-maid-alist-locally (ph lst)
  (let ((res '()) aux resaux (nlst '()))
    (cond
      ((null ph))
      ((listp (car ph))
       (setq ; process the internal with no 'lst' since it is optional:
	     aux (sli-get-maid-alist-locally (car ph) '())
             ; Then process the remainder with both candidates 'lst' and (cadr aux):
             resaux (sli-get-maid-alist-locally (cdr ph) (append (cadr aux) lst))
             ; glue things together:
             res (list (append aux (car resaux)) (cadr resaux))))
      (t (setq aux (elt (car ph) 0) ; the new 'last-word (lst=(last-word))
               ph (cdr ph))
         ; Link 'lst' to the new compulsory:
         (mapcar (lambda (s) (add-to-list 'res (cons s aux))) lst)
	 (while (and (not (null ph)) (listp (car ph)))
           ; (car ph) is an optional construct. Scan it with no 'lst'
           (setq resaux (sli-get-maid-alist-locally (car ph) '())
                 ; gather all 'last-words':
                 nlst (append nlst (cadr resaux))
                 ; gather all bindings :
                 res (append res (car resaux))
		 ph (cdr ph)))
	 (when (car ph) ; aux is linked to the new guy:
	   (add-to-list 'res (cons aux (elt (car ph) 0)))
           ; the new guy is linked with all the 'last-words':
	   (mapcar (lambda (s) (add-to-list 'res (cons s (elt (car ph) 0)))) nlst))
         ; process things farther:
         (setq resaux (sli-get-maid-alist-locally ph '())
               res (list (append (car resaux) res)
                         (if (null (cadr resaux)) (append (list aux) nlst)
                             (cadr resaux))))))
     res))

(defsubst sli-full-stuff (key alist fn1 fn2)
  (let ((res '()) aux)
    (while alist
      (when (setq aux (funcall fn1 (funcall fn2 key alist)))
        (add-to-list 'res aux))
      (setq alist (cdr alist)))
    res))

(defsubst sli-full-assoc (key alist)
  "The list of cdrs in alist whose car is key."
  (sli-full-stuff key alist 'cdr 'assoc))

(defsubst sli-full-rassoc (key alist)
  "The list of cars in alist whose cdr is key."
  (sli-full-stuff key alist 'car 'rassoc))

(defun sli-get-maid-alist nil
;; sli-ambiguous-keys is also created here.
  ;(setq sli-ambiguous-keys nil)
  (let ((res '()))
    (mapcar
      (lambda (ph)
        (setq res (append res (car (sli-get-maid-alist-locally ph '())))))
      sli-structures)
    (add-to-list 'res (cons block-comment-start block-comment-end))
    ; well, soft keys may correspond to different strong keys...
    (mapcar (lambda (co) (let ((to (sli-full-assoc co res)))
                           (cons co (if (null (cdr to)) (car to)
                                        (progn
                                          (add-to-list 'sli-ambiguous-keys co)
					  to)))))
      (sli-compact-list (sort (mapcar 'car res) 'string-lessp)))))

(defun sli-get-special-head-alist nil
  (let ((res '()))
    (mapcar
     (lambda (ph)
       (if (equal (elt ph 1) 'special-head)
           (add-to-list 'res (cons (elt ph 0) (elt ph 3)))))
     (sli-flatten sli-structures))
    res))

(defun sli-get-max-keys-length (lst)
  (let ((res 0))
    (mapcar (lambda (to) (setq res (max res to)))
            (mapcar 'length lst))
    res))

(defun sli-precomputations nil
  ;; variables:
  ;(print "Precomp: variables")
  (setq sli-head-keys (sli-scan-structures 'head)
        sli-special-head-keys (sli-scan-structures 'special-head)
        sli-soft-keys (sli-scan-structures 'soft)
        sli-beacon-keys (sli-scan-structures 'beacon)
        sli-math-relation-keys (sli-scan-structures 'math-relation)
        sli-relation-keys (append sli-beacon-keys sli-math-relation-keys)
        sli-strong-keys (sli-scan-structures 'strong)
        sli-end-keys (sli-scan-structures 'end)
        sli-keys-nomrelations (append sli-head-keys sli-soft-keys sli-strong-keys sli-beacon-keys
				      sli-special-head-keys ;; momentanous !! 
                                      sli-end-keys)
	sli-keys (append sli-keys-nomrelations sli-relation-keys)
        sli-max-keys-length (sli-get-max-keys-length sli-keys))
  ;;regexps:
  ;(print "Precomp: regexps")
  (setq sli-all-end-strong-regexp (sli-regexp-opt (append sli-end-keys sli-strong-keys))
        sli-fixed-regexp (sli-regexp-opt (mapcar 'car sli-fixed-keys-alist))
        sli-all-keys-nomrelations-regexp
          (sli-regexp-opt (append sli-keys-nomrelations sli-separators sli-comment-starts
                                 (list "\"" block-comment-start block-comment-end)))
        sli-all-keys-regexp
          (sli-regexp-opt (append sli-keys sli-separators sli-comment-starts
                                  (list "\"" block-comment-start block-comment-end))))
  ;; association lists:
  ;(print "Precomp: alists")
  (setq sli-ends-head-alist (sli-get-ends-head-alist)
	sli-head-end-alist (sli-get-head-end-alist)
	sli-heads-strong-alist (sli-get-heads-strong-alist) ; sli-ambiguous-keys also is partly created there.
        sli-companion-strong-keys-alist (sli-get-companion-alist)
        sli-soft-alist (sli-get-soft-alist)
        sli-soft-head-or-strong-alist (sli-get-soft-head-or-strong-alist)
	sli-special-head-alist (sli-get-special-head-alist)
        sli-relevant-alist (sli-get-relevant-alist)
        sli-ancestors-alist (sli-get-ancestors-alist)
	;; offsets :
        sli-first-offset-alist (sli-get-first-offset-alist)
        sli-second-offset-alist (sli-get-second-offset-alist)
        sli-relation-offset-alist (sli-get-relation-offset-alist)
        ;; the maid :
        sli-maid-alist (sli-get-maid-alist) ; sli-ambiguous-keys also is partly created there.
        )
  )

;;;--------------------------------------------------------------------------------------
;;; End of the section devoted to precomputations from sli-structures.
;;;--------------------------------------------------------------------------------------

;;;--------------------------------------------------------------------------------------
;;; This section is devoted to some simple functions extracting informations
;; from the variables defined above.
;;;--------------------------------------------------------------------------------------

  ;; A full-key is a cons (STRING . PT) where PT is the
  ;; value of point at the beginning of STRING.

(defsubst sli-following-key (key)
  (cdr (assoc key sli-maid-alist)))

(defun sli-indent-after (key &optional before-soft)
  (eval
   (cond
    ((and before-soft (member key (append sli-head-keys sli-strong-keys)))
     (cdr (assoc key sli-first-offset-alist)))
    ((member key (append sli-head-keys sli-strong-keys))
     (cdr (assoc key sli-second-offset-alist)))
    ((member key sli-relation-keys)
     (cdr (assoc key sli-relation-offset-alist)))
    ((member key sli-soft-keys)
     (cdr (assoc key sli-second-offset-alist)))
    ((member key sli-special-head-keys)
     (cdr (assoc key sli-second-offset-alist)))
    (t 0))))

(defsubst sli-get-shift (beg end)
  (or (eval (cdr (assoc (vector beg end) sli-shift-alist))) 0))

(defsubst sli-get-strongs-from-strong-or-head (strong)
  (cdr (assoc strong sli-companion-strong-keys-alist)))

(defsubst sli-get-head-from-end (end)
  (cdr (assoc end sli-head-end-alist)))

(defsubst sli-get-heads-from-strong (strong)
  (cdr (assoc strong sli-heads-strong-alist)))

(defsubst sli-get-ends-from-head (head)
  (cdr (assoc head sli-ends-head-alist)))

(defsubst sli-get-head-and-strong-from-soft (soft)
  (cdr (assoc soft sli-soft-alist)))

(defsubst sli-get-ends-from-strong (strong)
  (sli-flatten
   (mapcar 'sli-get-ends-from-head
           (sli-get-heads-from-strong strong))))

(defsubst sli-get-relevant (key)
  (cdr (assoc key sli-relevant-alist)))

(defsubst sli-possible-ancestors (key)
  (cdr (assoc key sli-ancestors-alist)))

;;;-------------------------------------------------------------------------------------------
;;; Some general primitives.
;;;-------------------------------------------------------------------------------------------

(defsubst sli-remove-trailing-spaces nil
  (if (looking-at "\\s-*\\($\\|\\'\\)") (delete-horizontal-space)))

(defsubst sli-only-spacep (&optional pt)
  (unless pt (setq pt (point)))
  (let ((only-spacep t))
    (mapcar (lambda (ch) (setq only-spacep
                               (and only-spacep (= (char-syntax ch) ? ))))
            (string-to-list
              (buffer-substring-no-properties (line-beginning-position) pt)))
    only-spacep))

(defsubst sli-point-to-indent (pt)
  (save-excursion
    (- pt (progn (goto-char pt) (beginning-of-line) (point)))))

(defsubst sli-indent-at (full-key)  ;; used only here
  ;; A full-key is a cons (STRING . PT) where PT is the
  ;; value of point at the beginning of STRING. PT alone is also accepted.
  (sli-point-to-indent (if (consp full-key) (car full-key) full-key)))

(defsubst sli-in-one-line-comment nil
  (re-search-backward (regexp-opt sli-comment-starts) (line-beginning-position) t))

(defsubst sli-get-safe-place nil
  (save-excursion
    (if (re-search-backward sli-safe-place-regexp nil t)
        (match-end 0) (point-min))))

(defsubst sli-within-long-comment nil
  (let*((aux (sli-get-safe-place))
	(res (parse-partial-sexp aux (point))))
    (and (nth 4 res) (not (nth 7 res)))))

(defun sli-anchored-posix-search-backward (regexp lim &optional no-error)
;;; ??? DOES NOT SEEM TO WORK  (posix-search-backward regexp lim no-error))
  (and (re-search-backward regexp lim no-error)
    (let*((end-pt (match-end 0))
          (beg (- end-pt sli-max-keys-length)))
      ;(print (list "Anchored posix. Candidate: " (match-beginning 0) (match-end 0)  " beg=" beg))
      ;(print (save-excursion (goto-char beg) (posix-search-forward regexp end-pt t)))
      (while (save-excursion
               (goto-char beg)
               (posix-search-forward regexp end-pt t)
               (< (match-end 0) end-pt))
        ;(print (list "Inside anchored posix: " (match-beginning 0) " beg=" beg))
        (setq beg (1+ beg)))
      ;(print (list "Out of anchored posix: " (match-beginning 0) " beg=" beg))
      (goto-char (match-beginning 0)))))

;;;---------------------------------------------------------------------------------
;;; The real stuff starts here.
;;;---------------------------------------------------------------------------------
;;
;;; Indentation
;;;

(defun sli-get-first-fixed-or-strong-or-end-or-soft (pt)
  ; Go to first non whitespace char on line on which PT lies and before PT.
  ; Then nil if within comment or first word is not a fixed/end/strong key,
  ; the cons (KEY . point-at-its-beginning) otherwise.
  (save-excursion
  (save-restriction
    (narrow-to-region (progn (beginning-of-line) (point)) pt)
    (skip-chars-forward " \t")
    (cond ((looking-at (regexp-opt (append sli-comment-starts (list block-comment-start))))
           (widen) (cons block-comment-start (point)))
          ((or (looking-at sli-fixed-regexp)
               (looking-at sli-all-end-strong-regexp)
               (looking-at (sli-regexp-opt sli-soft-keys)))
           (widen) (cons (match-string-no-properties 0) (point)))
          (t (widen) nil)))))

(defun sli-reduce-skel (skel &optional full)
  ; (cdr skel) is reduced if FULL is nil. With a t value,
  ; (cdr skel goes through reduction.
  (if (null skel) nil
   (let*((word (car skel)) end-lst strong-lst
         (found-strongp nil) (found-endp nil)
         (skel (if full (sli-reduce-skel (cdr skel) t) (cdr skel))))
     (cond
       ((member word sli-end-keys) ; don't do a thing !
        ;(print "yes")
        (append (list word) skel))
       ((member word sli-head-keys)
        ;; its end should be below or it is the key we seek. Erase this closed part.
        (setq end-lst (sli-get-ends-from-head word))
        (while (and skel (not (member (car skel) end-lst)))
          (setq skel (cdr skel)))
        (if (null skel) (list word) (cdr skel))) ; the answer.
       ((member word sli-strong-keys)  ;(print "??")
        ;; its end should be below or it is the key we seek.
        (setq end-lst (sli-get-ends-from-strong word)
              strong-lst (sli-get-strongs-from-strong-or-head word))
        (mapcar (lambda (s)
                  (setq found-endp (or found-endp (member s end-lst))
                        found-strongp (or found-strongp (member s strong-lst))))
                skel)
        (cond
         (found-endp
          (while (and skel (not (member (car skel) end-lst)))
            (setq skel (cdr skel))))
         ;; So word is a strong key with no end below.
         (found-strongp
          (while (and skel (not (member (car skel) strong-lst)))
            (setq skel (cdr skel)))
          (when (and (cdr skel) (member (cadr skel) strong-lst))
            (setq skel (cdr skel)))))
        (append (list word) skel))))))
  
(defun sli-find-matching-key (pt whatwewant relevant &optional givekey) ; goes backward
"PT is supposedly at beginning of an end/strong-key, out of comment or
string and we look for the first element of WHATWEWANT which is not
in a complete expression. RELEVANT is the list of keys that may
intervene.
That's a kind of backward-sexp...
Supports imbedded comments."
  (save-excursion
    (goto-char pt)
    ;(print (list "Getting in sli-find-matching-key with " pt whatwewant relevant))
    (let ((level-comment1 0) (skel '())
          (foundp nil) (ans nil) (case-fold-search nil)
          word start (in-stringp nil)
          (aregexp (sli-regexp-opt
                     (append relevant
                       (list "\"" block-comment-start block-comment-end)))))
      (while (and (not foundp) (not (bobp)))
        ;(print (list "Inside sli-find-matching-key. word " word "skel" skel))
        (if (sli-anchored-posix-search-backward aregexp nil 1)
          (cond
            ((string= (setq word (match-string-no-properties 0)) "\"")
             (if (= (preceding-char) ?\\)
                 (setq in-stringp t) ; it should already be.
               (setq in-stringp (not in-stringp))))
            (in-stringp)
            ; Out of strings:
            ((string= word block-comment-end)
             (setq level-comment1 (1+ level-comment1)))
            ((string= word block-comment-start)
             (setq level-comment1 (1- level-comment1)))
            ((member word sli-comment-starts)) ; within a one-line-comment
            ((> level-comment1 0)); within a multiline-comment
            ;; Out of imbedded comments. Now word is in RELEVANT.
            ((not (member word relevant))
             (setq foundp t ans nil))
            ((save-excursion (sli-in-one-line-comment)))
            (t (setq skel (sli-reduce-skel (append (list word) skel)))
               (when (and (= 1 (length skel)) (member (car skel) whatwewant))
                 (setq ans (if givekey (cons word (point)) (point))
                       foundp t)))) ; end of cond
       )) ; end of while
      ;(print (list "Out of sli-find-matching-key with " ans))
      ans)))

(defun sli-get-first-non-end-key (pt &optional nomrelation)
"Find first non-end-key before PT outside comment
or string which is not matched by an end-key.
Imbedded comments are supported.
If NOMRELATION is t, then this key is not a math-relation
either. Answer is a full-key (KEY, POINT)
where POINT indicates the beginning of the occurence
of KEY we're interested in.
Answer is (block-comment-start . point)
if PT is within a multiline-comment."
  (save-excursion
    (goto-char pt)
    (let ((level-comment1 0) (foundp nil)
          (accessible-separator (= (preceding-char) ?:))
          word start (in-stringp nil) (case-fold-search nil)
          (aregexp
             (if nomrelation sli-all-keys-nomrelations-regexp sli-all-keys-regexp)))
      (while (and (not foundp) (not (bobp)))
        (if (sli-anchored-posix-search-backward aregexp nil 1)
          (progn
            ;(print (list "Inside sli-get-first-non-end-key. word = "
            ;             (match-string-no-properties 0)))
          (cond
            ((string= (setq word (match-string-no-properties 0)) "\"")
             (if (= (preceding-char) ?\\)
                 (setq in-stringp t) ; it should already be.
               (setq in-stringp (not in-stringp))))
            (in-stringp)
            ;; Out of strings:
            ((string= word block-comment-end)
             (setq level-comment1 (1+ level-comment1)))
            ((string= word block-comment-start)
             (if (= level-comment1 0)
                 (setq foundp t)
               (setq level-comment1 (1- level-comment1))))
            ((member word sli-comment-starts)) ; within a one-line-comment
            ((> level-comment1 0)); within a multiline-comment
            ;; Out of imbedded comments:
            ((sli-is-a-separatorp)
             (setq start (point))
             (unless (sli-in-one-line-comment)
                     (goto-char start) (setq accessible-separator t)))
            ((member word sli-math-relation-keys) ; only if NOMRELATION is t.
             (unless accessible-separator
               (setq start (point))
               (unless (sli-in-one-line-comment)
                       (goto-char start) (setq foundp t))))
            ((member word sli-end-keys)
             (setq start (point))
             (unless (sli-in-one-line-comment)
               (goto-char
                (or
                 (sli-find-matching-key start (sli-get-head-from-end word) (sli-get-relevant word))
                 (point-min)))))
            ((member word sli-special-head-keys)
             (unless (or (sli-separator-directly-afterp pt
                              (cdr (assoc word sli-special-head-alist)))
                         (sli-in-one-line-comment))
                     (setq foundp t)))
            ((member word sli-separators))      ;; momentanous
            (t (setq foundp (not (sli-in-one-line-comment))))))
            ))
      ;(print (list "Out of sli-get-first-non-end-key with "
      ;         (if foundp (cons word (point)) nil) accessible-separator))
      (if foundp (cons word (point)) nil))))

(defun sli-get-corresponding-key (pt whatwewant)
  ; answer is (block-comment-start . point)
  ; if PT is within a multiline-comment.
  ; PT is at the beginning of the word we want to match.
  ; This function looks at all the keys, but skips
  ; head/end blocks by using sli-get-matching-key.
  ; Answers the first element of what we want that is not
  ; enclosed in a construct.
  (save-excursion
    (goto-char pt)
    (let ((level-comment1 0) (foundp nil)
          word start (in-stringp nil) (case-fold-search nil)
          (aregexp
            (sli-regexp-opt
              (append sli-keys sli-comment-starts
                      (list "\"" block-comment-start block-comment-end)))))
      ;(print (list "Getting in sli-get-corresponding-key."))
      (while (and (not foundp) (not (bobp)))
        (if (sli-anchored-posix-search-backward aregexp nil 1)
          (cond
            ((string= (setq word (match-string-no-properties 0)) "\"")
             (if (= (preceding-char) ?\\)
                 (setq in-stringp t) ; it should already be.
               (setq in-stringp (not in-stringp))))
            (in-stringp)
            ; Out of strings:
            ((string= word block-comment-end)
             (setq level-comment1 (1+ level-comment1)))
            ((string= word block-comment-start)
             (if (= level-comment1 0)
                 (setq foundp t)
               (setq level-comment1 (1- level-comment1))))
            ((member word sli-comment-starts)) ; within a one-line-comment
            ((> level-comment1 0)); within a multiline-comment
            ;; Out of imbedded comments:
            ((member word sli-end-keys)
             (setq start (point))
             (unless (sli-in-one-line-comment)
               (goto-char
                (or
                 (sli-find-matching-key start (sli-get-head-from-end word) (sli-get-relevant word))
                 (point-min)))))
            ((member word whatwewant)
             (setq start (point))
             (unless (sli-in-one-line-comment)
                     (setq foundp t)))
            (t nil))
           ))
       ;(print (list "Out of sli-get-corresponding-key with " (if foundp (cons word (point)) nil)))
      (if foundp (cons word (point)) nil))))

(defsubst sli-get-key-for-soft (pt soft)
  (sli-get-corresponding-key pt (sli-get-head-and-strong-from-soft soft)))

(defsubst sli-get-key-for-strong (pt strong)
  (sli-get-corresponding-key pt (sli-get-heads-from-strong strong)))

(defsubst sli-get-head-from-ambiguous (pt key)
  (let (auxkey)
    (cond
     ((member key sli-strong-keys)
      (sli-get-key-for-strong pt key))
     ((member key sli-soft-keys)
      (unless (member (car (setq auxkey (sli-get-key-for-soft (point) key))) sli-head-keys)
        (setq auxkey (sli-get-key-for-strong pt (car auxkey))))
      auxkey))))

(defun sli-separator-directly-afterp (end separator)
  "t if there is SEPARATOR between point and end
which is not within a comment or a string."
  (save-excursion
    ;(print (list "Getting in sli-separator-directly-afterp with " end))
    (let ((level-comment1 0) (level 0) (foundp nil)
           word (in-stringp nil))
      (while (and (not foundp) (< (point) end))
        (if (re-search-forward sli-all-keys-regexp end 1)
          (cond
            ((string= (setq word (match-string-no-properties 0)) "\"")
             (if (= (preceding-char) ?\\)
                 (setq in-stringp t) ; it should already be.
               (setq in-stringp (not in-stringp))))
            (in-stringp)
            ; Out of strings:
            ((string= word block-comment-end)
             (setq level-comment1 (1- level-comment1)))
            ((string= word block-comment-start)
             (setq level-comment1 (1+ level-comment1)))
            ((member word sli-comment-starts) (forward-line 1)) ; within a one-line-comment
            ((> level-comment1 0)); within a multiline-comment
            ;; Out of imbedded comments:
            ((and (string= word separator) (sli-is-a-separatorp (1- (point))))
             (setq foundp t)))
          ))
      ;(print (list "Out of sli-separator-directly-afterp. word =  " word))
      foundp)))

(defun sli-tell-indent (&optional afterp) ;; used only here
  "Gives the indentation of line on which point lies.
Or on line after if AFTERP is t."
  ;; This indentation depends on what is on the previous
  ;; line except that the first word of the line could be
  ;; a strong or end key in which case it is to be aligned
  ;; on the previous head/strong of the same block.
  ;; The only thing we don't do is if a string spreads across lines. 
  (sli-remove-trailing-spaces); for current-indentation
  (let*((pt (point)) wd-lst beg-str full-key appui head opp
        (first-stuff (and (not afterp) (sli-get-first-fixed-or-strong-or-end-or-soft pt))))
    ;(print (list "Getting in sli-tell-indent. first-stuff = " first-stuff afterp))
    (catch 'indent
    ; First case, indentation of this line and (car first-stuff) is a fixed key:
    (when (and (not (null first-stuff))
               (setq opp (assoc (car first-stuff) sli-fixed-keys-alist)))
      (throw 'indent (eval (cdr opp))))
    ; Second case, line starts by a soft key:
    ; it has to be done in case of "if 2<3 \n then" since the "then"
    ; has been aligned with respect to the math-relation and not to the "if"
    (when (and first-stuff (member (car first-stuff) sli-soft-keys))
      (setq appui (sli-get-key-for-soft (cdr first-stuff) (car first-stuff)))
      (throw 'indent (+ (sli-point-to-indent (cdr appui))
                        (sli-indent-after (car appui)))))
    ; Third case, indentation of this line and (car first-stuff) is not a fixed key or a comment:
    (when (and first-stuff (not (string= (car first-stuff) block-comment-start)))
      ; line starts by a strong/end key. We select the key from which to
      ; compute the indent. Usually we align it on the previous head/strong
      ; key and add possible offset. That's the heredity principle. But we can also
      ; align strong/end-keys on the head
      (setq appui
            (sli-find-matching-key   ; backward
             (cdr first-stuff) ; where to start the search.
             (sli-possible-ancestors (car first-stuff))
             (sli-get-relevant (car first-stuff)) t))
      ; see whether heredity applies:
      (unless (or (null appui) (member (car appui) sli-head-keys))
        ; select head from appui and not from full-key because
        ; (1) it is shorter (2) (car head) *is* a strong key.
        (setq head (sli-get-head-from-ambiguous (cdr appui) (car appui)))
        ;(print (list "heredity ? for " (vector (car head) (car first-stuff))))
        (when (member (vector (car head) (car first-stuff)) sli-no-heredity-list)
          (setq appui head)))
      (throw 'indent (if (null appui) 0
                       (+ (sli-get-shift (car appui) (car first-stuff))
                          (sli-indent-at (cdr appui))))))
    ; Fourth case, indentation of this line and (car first-stuff) is a comment:
    (when (and first-stuff (string= (car first-stuff) block-comment-start))
      ; PT is within multi-line-comment.
      ;(print "Within multi-line-comment")
      (throw 'indent (current-indentation)))

    (unless afterp
      ; ; Fifth case : line doesn't start by a strong/end/soft key:
      (save-excursion
        (if (= -1 (forward-line -1))
            ; we are already on the first line:
            (if first-stuff (throw 'indent (current-indentation))
                (throw 'indent 0)))
        (end-of-line)
        (setq pt (point))))

    ; This point can be reached only if AFTERP is t.
    (setq first-stuff (sli-get-first-non-end-key pt)) ; backward search
    ;(print (list "Inside sli-tell-indent. first-stuff = " first-stuff))

    (cond
      ((null first-stuff)
       ; no construct active or within comment. Don't do a thing:
       (throw 'indent (if afterp 0 (current-indentation))))
      ((string= (car first-stuff) block-comment-start)
       (throw 'indent (current-indentation)))
      ((and (member (car first-stuff) (append sli-head-keys sli-strong-keys))
            (not (assoc (car first-stuff) sli-soft-head-or-strong-alist)))
         ; head/strong without soft:
        (throw 'indent (+ (sli-point-to-indent (cdr first-stuff))
                          (sli-indent-after (car first-stuff)))))
      ((member (car first-stuff) (append sli-head-keys sli-strong-keys
                                         sli-special-head-keys))
       ; head/strong with soft missing or special-head:
       (throw 'indent (+ (sli-point-to-indent (cdr first-stuff))
                         (sli-indent-after (car first-stuff) t))))
      ((member (car first-stuff) sli-relation-keys)
       ; relation:
       (throw 'indent (+ (sli-point-to-indent (cdr first-stuff))
                         (sli-indent-after (car first-stuff)))))
      ((member (car first-stuff) sli-soft-keys)
       ; a soft key. Find its head/strong and align things on it.
       (setq full-key (sli-get-key-for-soft (cdr first-stuff) (car first-stuff)))
       (throw 'indent (+ (sli-point-to-indent (cdr full-key))
                         (sli-indent-after (car full-key)))))))))

;;;-----------------------------------------------------------------------
;;;  Functions that are used outside.
;;;-----------------------------------------------------------------------

(defsubst sli-insert-indent (ind)
  (or (null ind)
    (let ((beg (point)) last move-p (cc (current-indentation))
          (old-buff-modp (buffer-modified-p)))
      (save-excursion
        (setq last (- beg (progn (beginning-of-line) (point)))
              move-p (re-search-forward "[^ \t]" beg t))
        (beginning-of-line)
        (delete-horizontal-space) ; Simply because I Hate \t chars.
        (insert-char ?  ind))
      ; If ind is cc on unmodified buffer, declare the buffer as unmodified:
      (set-buffer-modified-p (or old-buff-modp (not (= cc ind))))
      ; if point was inside the removed spaces,
      ; then now it is at the beginning of the line.
      ; Not what we wanted.
      (unless move-p ; point has been moved automatically
        (forward-char ind)))))

(defun sli-indent-line nil ;; used repeateadly outside
  (save-excursion
    (end-of-line) (sli-insert-indent (sli-tell-indent))))

(defun sli-indent-region (beg end)
  (interactive "r")
  (save-excursion
    (setq end (progn (goto-char end) (end-of-line) (point)))
    (goto-char beg)
    (while (progn (sli-indent-line)
                  (and (re-search-forward "$" end t)
                       (not (= end (point)))))
      (forward-line 1))))

(defun sli-electric-tab nil ;; linked to 'indent-line-function
  "The interactive counterpart of 'sli-indent-line.
Does a number of other things: 
 -- if there are nothing but spaces between beginning-of-line
    and (point), then indents the line and sends (point)
    to the first non space ot tab character of the line.
 -- else if sli-tab-always-indent then indents the line
    the cursor being 'relatively' fixed.
In a program, use `sli-indent-line'."
  (interactive)
  (narrow-to-region (sli-get-safe-place) (point-max))
  (if (sli-only-spacep)
      (progn
        (sli-indent-line)
        (skip-chars-forward " \t"))
    (when sli-tab-always-indent (sli-indent-line)))
  (widen))

(defun sli-electric-terminate-line (&optional beg)
  "Terminate line and indent next line."
  (interactive)
  (if (sli-within-long-comment)
    (insert-char ?\n 1)
    (narrow-to-region (sli-get-safe-place) (point-max))
    (let (next-indent only-spacep)
      (sli-remove-trailing-spaces)
      (setq only-spacep (sli-only-spacep))
      ;(print (list "only-spacep = " only-spacep))
      (sli-insert-indent (sli-tell-indent))
      (unless only-spacep (insert-char ?  1)) ; (print "Youp")
             ;--> in case of thendo with point between then and do.
      (setq next-indent (sli-tell-indent t))
      ;(print (list "next-indent" next-indent))
      (unless only-spacep (if (= (char-syntax (preceding-char)) ? )(delete-char -1)))
      (insert-char ?\n 1)
      (sli-insert-indent next-indent))
     (widen)))

(defun sli-newline (&optional beg)
  "Insert a newline without indenting current line.
Next lien is properly indented."
  (interactive)
  (if (sli-within-long-comment)
      (insert-char ?\n 1)
    (narrow-to-region (sli-get-safe-place) (point-max))
    (sli-remove-trailing-spaces)
    (insert-char ?\n 1)
    (sli-insert-indent (sli-tell-indent))
    (widen)))

(defun sli-maid (&optional arg)
  "Closes constructs for you, puts the children to bed and
may order a pizza if you know how to ask.
 Usually, adds the corresponding part of `sli-add-to-key-alist'
except when the call is prefixed by C-u. If the variable
`sli-more-maidp' is nil, this behaviour is reversed."
  (interactive "P")
  (narrow-to-region (sli-get-safe-place) (point-max))
  (let ((full-key (sli-get-first-non-end-key (point) t)) (key nil) (head nil) smore)
    (sli-remove-trailing-spaces)
    ; Sort ambiguity arising from ambiguous-keys:
    (when (and full-key (member (car full-key) sli-ambiguous-keys))
      (setq head (car (sli-get-head-from-ambiguous (cdr full-key) (car full-key)))))
    ; Go out of one-line-comment:
    (when (save-excursion (sli-in-one-line-comment))
      (sli-electric-terminate-line))
    ; add a newline before insertion if required:
    (unless (sli-only-spacep)
      (when (and full-key (member (car full-key) sli-keys-with-newline))
            (sli-electric-terminate-line)))
    ;(print (list "Inside mupad-maid. full-key/head = " full-key head))
    ; find or insert closing-key:
    (cond
      ((null full-key)
       ; No construct to be closed.
       (setq key (buffer-substring-no-properties
                   (save-excursion (forward-word -1) (point)) (point))))
      ((equal (car full-key) block-comment-start)
       (insert (setq key block-comment-end)))
      ((and (member (car full-key) sli-separators)
            ; Beware !! this key could be **very far**
            (= (count-lines (cdr full-key) (point)) 0))
       (setq key nil)) ; We shall put a newline, see below.
      ((and (member (car full-key) sli-special-head-keys)
            (not (sli-separator-directly-afterp (cdr full-key)
                      (setq key (cdr (assoc (car full-key) sli-special-head-alist))))))
       (insert key))
      (t (setq key (if head ;  completion of an ambiguous-key:
                       (car (sli-get-ends-from-head head))
                       (sli-following-key (car full-key))))
         ; add a space if required:
         (unless (and (not (null key))
                      (or (not (= (char-syntax (string-to-char key)) ?w))
                          (= (char-syntax (preceding-char)) ? )))
                 (insert-char ?  1))
         (or (null key) (insert key))))
    ;(print (list "Inside mupad-maid. key = " key))
    ; add things if required:
    (unless (if sli-more-maidp
                (and arg (= (car arg) 4)) ; call is  prefixed by C-u
                (not (and arg (= (car arg) 4)))) ; call is not prefixed by C-u
      (cond
        ((null key))
        ((setq smore (assoc key sli-add-to-key-alist)) (insert (cdr smore)))))
    ; add a newline if required:
    (cond
      ((eobp) (sli-electric-terminate-line))
      ((or (null key)
           (< 2 (count-lines (point)
                   (save-excursion (skip-chars-forward " \t\n") (point)))))
       (sli-indent-line) (forward-line 1) (sli-indent-line)) ; beware if it is
                                                             ; only an empty line.
      (t (sli-indent-line))))
  (widen))

(defun sli-tools (struct shift sep sepp fixed safe keyn mkey comm noher)
"Once these tools are loaded, you should have
`sli-newline' and `sli-electric-terminate-line'
which behave like `newline-and-indent' and
`reindent-then-newline-and-indent'. Also
`indent-line-function' is `sli-electric-tab'
and
`indent-region-function' is `sli-indent-region'.

Finally, `sli-maid' tries to further constructs
for you.

For these tools to work, the parameters are
`sli-structures'
`sli-shift-alist'
`sli-separators'
`sli-is-a-separatorp-fn'
`sli-fixed-keys-alist'
`sli-safe-place-regexp'
`sli-keys-with-newline'
`sli-add-to-key-alist'
`sli-comment-starts'
`sli-no-heredity-list'
and you should also set
`block-comment-start'      `block-comment-end'
`sli-more-maidp'
and the syntax table should be ok."
  (interactive)
  (setq sli-structures struct
        sli-shift-alist shift
        sli-separators sep
        sli-is-a-separatorp-fn sepp
        sli-fixed-keys-alist fixed
        sli-safe-place-regexp safe
        sli-keys-with-newline keyn
        sli-add-to-key-alist mkey
        sli-comment-starts comm
        sli-no-heredity-list noher)
        
  (set (make-local-variable 'indent-line-function) 'sli-electric-tab)
  (set (make-local-variable 'indent-region-function) 'sli-indent-region)
  (sli-precomputations))

;;------------------ sli-tools ends here. 1343 ??