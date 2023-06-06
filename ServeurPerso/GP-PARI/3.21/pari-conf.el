;; pari-conf.el or pari-conf.el.in -- configuration file.

;; Copyright (C) 1997-2017  The PARI group.

;; This file is part of the PARIEMACS package.

;; PARIEMACS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation. It is distributed in the hope that it
;; will be useful, but WITHOUT ANY WARRANTY WHATSOEVER.

;; Check the License for details. You should have received a copy of
;; it, along with the package; see the file 'COPYING'. If not, write
;; to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Maintainer (01-March-2003): Olivier Ramare (olivier.ramare AT univ-amu.fr).
;; To be used with pari.el version 3.05 or higher.
;; Version: 3.14

(provide 'pari-conf)
;; Set the following constants for your site:
(defconst gp-version "2.12.0" "pari's version number")

(defconst gp-file-name "/usr/bin/gp"
 "The file name of the gp executable file")

(defconst gp-gphelp-dir "/usr/bin/"
  "The directory where gphelp is to be found")

(defconst gp-pariemacs "/home/ramare/lisp/latest-pari-distrib/README"
  "The pariemacs file")

(defconst gp-flag-for-emacs 
  "--emacs"  ;; if version >= 2.3.0
;;  "-emacs"   ;; if version < 2.3.0
  "gp version >= 2.3.0 uses the flag --emacs instead of the earlier -emacs"
)  
