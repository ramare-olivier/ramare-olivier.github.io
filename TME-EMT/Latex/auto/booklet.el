(TeX-add-style-hook
 "booklet"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "10pt" "twoside" "svgnames")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("biblatex" "backend=biber" "style=numeric-comp" "citetracker=true" "pagetracker=true" "hyperref=true" "backref=true" "firstinits=true" "bibencoding=utf8") ("hyperref" "colorlinks=true" "pdftex")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "Art01"
    "Art02"
    "Art10"
    "Art12"
    "Art03"
    "Art04"
    "Art16"
    "Art17"
    "Art05"
    "Art11"
    "Art07"
    "Art15"
    "Art06"
    "Art08"
    "Art09"
    "Art14"
    "Art18"
    "Art13"
    "Art19"
    "book"
    "bk10"
    "amsfonts"
    "amsmath"
    "amssymb"
    "url"
    "latexsym"
    "mathrsfs"
    "csquotes"
    "biblatex"
    "placeins"
    "epic"
    "graphicx"
    "epstopdf"
    "pstricks"
    "pst-plot"
    "hyperref"
    "fancyhdr")
   (LaTeX-add-environments
    '("thm" 1))
   (LaTeX-add-bibliographies
    "Local-TME-EMT")
   (LaTeX-add-pagestyles
    "plain"))
 :latex)

