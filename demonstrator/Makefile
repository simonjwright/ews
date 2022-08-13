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

# Makefile for EWS demonstration.

# For Alire puroses, only the demo_code target applies; it's called in
# a pre-build action, while the actual build is done by alr
# build. This is needed because
#
# - the actual GPR is created by nuweb from ews.py
# - Alire needs a GPR (here, and by default, demonstrator.gpr)
# - demonstrator.gpr is an aggregate project which calls up the generated
#   project.

all: demo demo_code pdf

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
GENERATOR = ../bin/ews_generator

$(GENERATOR):
ifeq ($(ALIRE),)
	cd ../generator; gprbuild -P ews_generator
endif

CODE_GENERATED = \
  ews_demo.gpr 					\
  ews_demo.adb 					\
  ajax.html 					\
  ajax.js

# Work round missing 'grouped target' in GNU make < 4.3.
$(CODE_GENERATED): code-generated-stamp ;
code-generated-stamp: ews.w
	$(NUWEB) -t $<
	touch $@

EWS_GENERATED = ews_htdocs.ads ews_htdocs.adb

# This actually depends on ews.pdf, but if we mention it here it'll be
# rebuilt (well, it'll almost certainly fail to be rebuilt (unless the
# user has pdflatex installed))
$(EWS_GENERATED): ews-generated-stamp ;
ews-generated-stamp: $(GENERATOR)		\
  index.html					\
  ajax.html					\
  ajax.js					\
  HttpInteraction.js				\
  favicon.ico
	$(GENERATOR)
	touch $@

HttpInteraction.js: ../lib/HttpInteraction.js
	cp $< $@

favicon.ico: ../lib/favicon.ico
	cp $< $@

demo: ews_demo$(EXE)
.PHONY: demo

demo_code:: $(CODE_GENERATED) $(EWS_GENERATED)
.PHONY: demo_code

pdf: ews.pdf
.PHONY: pdf

ews_demo$(EXE): demo_code
	gprbuild -P ews_demo

ews.pdf: ews.w
	$(NUWEB) -r ews
	$(PDFLATEX) ews
	$(NUWEB) -r ews
	$(PDFLATEX) ews

clean:
	-gprclean -P ews_demo
	$(DEL) ews.aux ews.lof ews.log ews.out ews.tex ews.toc
	$(DEL) ews_demo* ews_htdocs.*
	$(DEL) *-stamp
	$(DEL) HttpInteraction.js favicon.ico ajax.html ajax.js
.PHONY: clean
