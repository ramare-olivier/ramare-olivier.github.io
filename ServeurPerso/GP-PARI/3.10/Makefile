#===========================================================================
# You may need to modify one of the following three variables

VERSION=3.10

# Path to pari.cfg file.
# Several possibilites:
PARI_CFG=/usr/local/lib/pari/pari.cfg
# For Debian (package "libpari-dev") install: 
#PARI_CFG=/usr/lib/pari/pari.cfg
# For some other install, the architecture is mentioned 
# (see https://wiki.ubuntu.com/MultiarchSpec):
#PARI_CFG=/usr/lib/i386-linux-gnu/pari/pari.cfg
#PARI_CFG=/usr/lib/x86_64-linux-gnu/pari/pari.cfg

# In case, you cannot find it, you can use the following default file
# (but edit this file first): 
#PARI_CFG=./pari.cfg-default

# Where to install ourselves ?
INSTALL=/usr/local/share/emacs/site-lisp/pari

# Path to 'emacs' binary
EMACS=emacs

#===========================================================================
# Maintainer only.

# version number (How do we call ourselves?)
DIR=pariemacs-$(VERSION)
# note that the pariemacs file was called $(DIR).txt
# and is now called README, with  the addition of the README file.
FILES=Makefile pari-completion.el pari-conf.el.in pari-fontification.el pari-help.el pari-messages.el pari.el pari-history.el README CHANGES COPYING sli-tools.el pari.cfg-default pariemacs-$(VERSION).txt
TARFILE=$(DIR).tar

install: pari-conf.el elc
	mkdir -p $(INSTALL)
	cp *.elc *.el README CHANGES COPYING $(INSTALL)

pari-conf.el: 
	@if test ! -f $(PARI_CFG); then \
	  echo "-----\nNo file$(PARI_CFG) found.\n\nEdit file pari.cfg-default\nand change PARI_CFG of the file Makefile to point to it\n---"; exit 1; \
	fi
	. $(PARI_CFG); sed -e "s/@version@/$$pari_release/" \
	    -e "s!@emacsdir@!$(INSTALL)!" \
	    -e "s!@bindir@!$$bindir!" pari-conf.el.in > $@

elc:
	@echo "Byte-Compiling elisp files..."
	-$(EMACS) -batch \
	   --eval '(setq load-path (append (list ".") load-path))' \
	   -f batch-byte-compile *.el

distrib: 
	@if test -d $(DIR); then \
	  echo "Remove $(DIR) before building a new release"; exit 1; \
	fi
	@tar cf $(TARFILE) $(FILES)
	@mkdir $(DIR) && mv $(TARFILE) $(DIR)
	@cd $(DIR) && tar xf $(TARFILE) && rm -f $(TARFILE)
	@tar cf $(TARFILE) $(DIR)
	@rm -rf $(DIR)
	@rm -f $(TARFILE).gz
	@gzip  $(TARFILE)

clean:
	rm -f *.elc pari-conf.el
