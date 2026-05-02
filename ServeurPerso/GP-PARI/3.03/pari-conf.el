;; To be used with pari.el version 3.02 or higher.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'pari-conf)
;; Set the following constants for your site:
(defconst gp-version "2.4.3 (development svn-11900)" "pari's version number")

(defconst gp-file-name "/usr/local/svn_pari_install/bin/gp"
 "The file name of the gp executable file")

(defconst gp-gphelp-dir "/usr/local/svn_pari_install/bin/"
  "The directory where gphelp is to be found")

(defconst gp-pariemacs "@miscdir@/emacs/README"
  "The pariemacs file")
