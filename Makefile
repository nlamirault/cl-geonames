#
# Makefile for cl-lastfm
#


SBCL=`which sbcl-git`


all: geonames


help:
	@echo "CL-GEONAMES (c) Nicolas Lamirault"
	@echo "  dist    : creates a distribution archive"
	@echo "  doc     : Creates API documentation"
	@echo "  cover   : Calculate code coverage"
	@echo "  web     : Generate website in the 'www' directory" 
	@echo "  clean   : clean development environnement"
	@echo "  cleanup : general clean directory"

dist:
	@echo "Create distribution"

doc:
	@echo "Make API documentation"
	$(SBCL) --script misc/geonames-atdoc.lisp

cover:
	@echo "Code coverage"
	$(SBCL) --dynamic-space-size 500 --script misc/geonames-cover.lisp

web: www/index.xsl www/index.css
	@echo "Make website"
	cd www; $(MAKE)

clean:
	find . \( -name "*.fasl" -o -name "lift.dribble" \) -print -exec rm -fr {} \;
	find www -name "index.html" -print -exec rm -fr {} \;
	touch www/index.xsl
	touch www/index.xml

cleanup: clean
	rm -fr www/api
	rm -fr www/cover


geonames: doc cover web
