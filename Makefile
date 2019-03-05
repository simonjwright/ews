# Copyright Simon Wright <simon@pushface.org>

# This file is part ofthe embedded web server EWS, hosted at Sourceforge
# by Simon Wright.

# EWS is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.  It is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not, see
# <http://www.gnu.org/licenses/>.

all::
.PHONY: all

GPRBUILD ?= gprbuild

# Subdirectories, each implements its own contribution.
SUBDIRS = doc src

# Libraries

all:: lib-static-stamp lib-relocatable-stamp exec-stamp

lib-static-stamp: ews.gpr
	$(GPRBUILD) -p -P ews.gpr -XLIBRARY_TYPE=static
	touch $@

lib-relocatable-stamp: ews.gpr
	$(GPRBUILD) -p -P ews.gpr -XLIBRARY_TYPE=relocatable
	touch $@

# Executables

exec-stamp: lib-static-stamp make_htdocs.gpr
	$(GPRBUILD) -p -P make_htdocs.gpr -XLIBRARY_TYPE=static
	touch $@

# Demos

demo:
	for s in $(SUBDIRS); do \
	  $(MAKE) -w -C $$s $@; \
	done
.PHONY: demo

# Installation

prefix ?= $(realpath $(dir $(shell which gnatls))/..)

install: install-static-lib install-relocatable-lib install-exec
.PHONY: install

install-static-lib: lib-static-stamp
	gprinstall				\
	  -P ews.gpr				\
	  --install-name=ews			\
	  --prefix=$(prefix)			\
	  --mode=dev				\
	  --project-subdir=lib/gnat		\
	  --build-var=LIBRARY_TYPE		\
	  --build-name=static			\
	  -XLIBRARY_TYPE=static			\
	  -f					\
	  -p
.PHONY: install-static-lib

install-relocatable-lib: lib-relocatable-stamp
	gprinstall				\
	  -P ews.gpr				\
	  --install-name=ews			\
	  --prefix=$(prefix)			\
	  --mode=dev				\
	  --project-subdir=lib/gnat		\
	  --build-var=LIBRARY_TYPE		\
	  --build-name=relocatable		\
	  -XLIBRARY_TYPE=relocatable		\
	  -f					\
	  -p
.PHONY: install-relocatable-lib

install-exec: exec-stamp
	gprinstall				\
	  -P make_htdocs.gpr			\
	  --install-name=make_htdocs		\
	  --prefix=$(prefix)			\
	  -XLIBRARY_TYPE=static			\
	  -f					\
	  -p
.PHONY: install-exec

clean:
	-gnatclean -P make_htdocs.gpr
	-gnatclean -P ews.gpr -XLIBRARY_TYPE=static
	-gnatclean -P ews.gpr -XLIBRARY_TYPE=relocatable
	rm -f *-stamp
	for s in $(SUBDIRS); do \
	  $(MAKE) -w -C $$s $@; \
	done
.PHONY: clean

# Distribution.

# Create the current date, in the form yyyymmdd.
RELEASE ?= $(shell date +%Y%m%d)

# Files to copy to the distribution (Makefile-dist done specially)
FILES = COPYING3 COPYING.RUNTIME INSTALL README ews.gpr make_htdocs.gpr

# Distribution directory - eg ews-20130705
DISTDIR = ews-$(RELEASE)

dist: $(DISTDIR).tar.gz $(DISTDIR).zip
.PHONY: dist

$(DISTDIR).tar.gz: $(DISTDIR)
	tar zcvf $@ $<

$(DISTDIR).zip: $(DISTDIR)
	zip -lr $@ $</*

$(DISTDIR): $(FILES) Makefile-dist
	-rm -rf $@
	mkdir $@
	cp $(FILES) $@/
	cp Makefile-dist $@/Makefile
	for s in $(SUBDIRS); do \
	  $(MAKE) DIST=$(PWD)/$@ -C $$s dist; \
	done
