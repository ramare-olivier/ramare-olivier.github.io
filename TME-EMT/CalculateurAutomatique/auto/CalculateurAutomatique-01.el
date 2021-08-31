(TeX-add-style-hook
 "CalculateurAutomatique-01"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "10pt" "twoside" "leqno")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "amsmath"
    "amsthm"
    "amssymb"
    "latexsym"
    "enumerate"
    "url")
   (LaTeX-add-environments
    "thm"
    "lem"
    "cor"))
 :latex)

