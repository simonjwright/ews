# Copyright (C) 2004-2022 Simon Wright <simon@pushface.org>

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

all: demo

NUWEB ?= ./nuweb.py
PDFLATEX ?= pdflatex
ifneq (,$(findstring mingw,$(shell gcc -dumpmachine)))
  EXE = .exe
endif
ifeq ($(OS),Windows_NT)
  DEL = del
else
  DEL = rm -f
endif

demo: ews_demo$(EXE)
.PHONY: demo

NUWEB_GENERATED =				\
  ews.pdf 					\
  ews_demo.gpr 					\
  ews_demo.adb 					\
  ajax.html 					\
  ajax.js

EWS_GENERATED = ews_htdocs.ads ews_htdocs.adb

$(EWS_GENERATED):				\
  index.html 					\
  ews.pdf					\
  ajax.html					\
  ajax.js					\
  HttpInteraction.js				\
  favicon.ico
	../bin/ews-make_htdocs

HttpInteraction.js: ../lib/HttpInteraction.js
	cp $< $@

favicon.ico: ../lib/favicon.ico
	cp $< $@

# We need to use a stamp because nuweb only touches a file (and
# therefore changes the date) if the content has changed.
$(NUWEB_GENERATED): nuweb-generated-stamp
# We have to do this twice to get cross-references correct.
nuweb-generated-stamp: ews.w
	$(NUWEB) -r ews
	$(PDFLATEX) ews
	$(NUWEB) -r ews
	$(PDFLATEX) ews
	touch $@

ews_demo$(EXE): ews_demo.gpr ews_demo.adb $(EWS_GENERATED) $(NUWEB_GENERATED)
	gprbuild -P ews_demo

clean:
	-gprclean -P ews_demo
	$(DEL) ews.aux ews.lof ews.log ews.out ews.pdf ews.tex ews.toc
	$(DEL) ews_demo* ews_htdocs.*
	$(DEL) nuweb-generated-stamp
	$(DEL) HttpInteraction.js favicon.ico ajax.html ajax.js
.PHONY: clean
