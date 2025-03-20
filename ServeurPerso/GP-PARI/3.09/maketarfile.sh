#!/bin/sh
# To be used in the same directory as all the files.
cp README pariemacs-3.09.txt

tar cvf pariemacs-3.09.tar README CHANGES COPYING Makefile pari.el pari-completion.el pari-conf.el.in pariemacs-3.09.txt pari-fontification.el pari-help.el pari-history.el pari-messages.el sli-tools.el maketarfile.sh pari.cfg-default

gzip -9 pariemacs-3.09.tar
exit 0
