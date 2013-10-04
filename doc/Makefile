#  Copyright (C) Simon Wright <simon@pushface.org>

# EWS is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.  It is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not, see
# <http://www.gnu.org/licenses/>.

# Makefile for EWS documentation.

NUWEB ?= ~/nuweb.py/nuweb.py
PDFLATEX ?= pdflatex

all::

NUWEB_GENERATED = ews.pdf ews_demo.gpr ews_demo.adb ajax.html ajax.js

EWS_GENERATED = ews_htdocs.ads ews_htdocs.adb

$(EWS_GENERATED): index.html ajax.html ajax.js HttpInteraction.js favicon.ico
	../bin/ews-make_htdocs

HttpInteraction.js: ../lib/HttpInteraction.js
	cp $< $@

favicon.ico: ../lib/favicon.ico
	cp $< $@

# We have to do this twice to get cross-references correct.
$(NUWEB_GENERATED): ews.w
	$(NUWEB) -r ews
	$(PDFLATEX) ews
	$(NUWEB) -r ews
	$(PDFLATEX) ews

ews_demo: ews_demo.gpr ews_demo.adb $(EWS_GENERATED)
	gnatmake -p -P ews_demo

dist:
	mkdir $(DIST)/doc
	cp $(FILES) $(DIST)/doc/
.PHONY: dist

USER = simonjwright
PRJ = embed-web-srvr
FILES = index.html

upload: index.html ews.pdf
	rsync -av $< $(USER),$(PRJ)@web.sourceforge.net:htdocs/
.PHONY: upload

clean:
	-rm -rf .build
	-rm ews.{aux,lof,log,out,pdf,tex,toc}
	-rm ews_demo* ews_htdocs.*
.PHONY: clean
