(TeX-add-style-hook
 "Excursions errata 4"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("amsart" "12pt" "reqno")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "amsart"
    "amsart12"
    "graphicx"
    "amssymb"
    "epstopdf"
    "hyperref"
    "mathrsfs")
   (TeX-add-symbols
    '("conj" 1)
    '("norm" 1)
    '("abs" 1)
    "Z"
    "R"
    "Q"
    "C"
    "N"
    "divides"
    "notdivides")
   (LaTeX-add-bibliographies
    "AllenMain")
   (LaTeX-add-amsthm-newtheorems
    "theorem"
    "lemma"
    "proposition"
    "definition"
    "example"
    "exercise"
    "conjecture"
    "case"))
 :latex)

