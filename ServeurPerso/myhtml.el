(defun myhtml-define-alternate-tags (tag)
  (list tag))

(defun mymtml-makes-simple-structures (tag-list)
  "TAG-LIST is a list of downcase words."
  (let ((res nil) tags)
    (mapcar
     (lambda (tag)
       (setq tags (myhtml-define-alternate-tags tag))
       (let ((aux nil))
         (mapcar 
          (lambda (ahead)
            (mapcar
             (lambda (aend)
               (setq aux
                     (append aux 
                             (list (list (vector (concat "<" ahead) 'head 1)
                                         (vector (concat "</" aend ">") 'end))))))
             tags))
          tags)
         (setq res (append res aux))))
     tag-list)
    res))

(add-hook 'html-mode-hook
          '(lambda nil
             (require 'sli-tools)
             (let ((html-structures
                    (mymtml-makes-simple-structures
                     '("font" "body" "comment" "frameset" "frame" "noframes" "head" "html"
                       "style" "title" "a" "b" "big" "blink" "code" "del" "i" "s" "small"
                       "strike" "sub" "sup" "tt" "u" "abbr" "acronym" "cite" "dfn" "em"
                       "kbd" "samp" "strong" "var" "map" "marquee" "script" "server" 
                       "address" "bdo" "blockquote" "center" "div" "font" "h1" "h2" "h3"
                       "h4" "h5" "h6" "iframe" "ilayer" "layer" "listing" "multicol"
                       "nobr" "pre" "q" "span" "xmp" "li" "dir" "menu" "ol" "ul" "table"
                       "form" "button" "fieldset" "label" "legend" "select" 
                       ;; omitted : "p"
                       "option" "optgroup" "textarea" "table" "caption" "tbody"
                       "tfoot" "thead" "tr" "td" "th" "applet" "param")))
                   (html-shift-alist nil)
                   (html-separators nil)
                   (html-is-a-separatorp nil)
                   (html-fixed-keys-alist nil)
                   (html-safe-place-regexp "\\(\\'\\|\\`\\)")
                   (html-keys-with-newline nil)
                   (html-keys-without-newline nil)
                   (html-add-to-key-alist nil)
                   (html-comment-start nil)
                   (html-no-heredity-list nil)
                   (html-newline nil)
                   (html-correction-alist nil)
                   (html-show-sexpp nil)
                   (html-case-fold t)
                   (html-eeov '(lambda (key pt)
                                 (save-excursion
                                   (goto-char (+ pt (length key)))
                                   (unless (equal (preceding-char) ?>)
                                     (re-search-forward ">" nil t))
                                   (point)))))
               (setq sli-tab-always-indent nil
                     sli-more-maidp nil
                     block-comment-start "<!--"
                     block-comment-end "-->")
               ;(modify-syntax-entry ?< "_")
               ;(modify-syntax-entry ?> "_")
               (modify-syntax-entry ?/ "_")
               (sli-tools html-structures
                          html-shift-alist
                          html-separators
                          html-is-a-separatorp
                          html-fixed-keys-alist
                          html-safe-place-regexp
                          html-keys-with-newline
                          html-keys-without-newline
                          html-add-to-key-alist
                          html-comment-start
                          html-no-heredity-list 
                          html-newline
                          html-correction-alist 
                          html-show-sexpp
                          html-case-fold
                          html-eeov)
               (local-set-key [(control ?c) (control ?e)] 'sli-maid))))

