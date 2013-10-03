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

GPRBUILD ?= gprbuild

all: lib-static-stamp lib-relocatable-stamp exec-stamp
.PHONY: all

lib-static-stamp: ews.gpr
	$(GPRBUILD) -p -P ews.gpr -XLIBRARY_TYPE=static
	touch $@
.PHONY: lib-static-stamp

lib-relocatable-stamp: ews.gpr
	$(GPRBUILD) -p -P ews.gpr -XLIBRARY_TYPE=relocatable
	touch $@
.PHONY: lib-relocatable-stamp

exec-stamp: lib-static-stamp make_htdocs.gpr
	$(GPRBUILD) -p -P make_htdocs.gpr -XLIBRARY_TYPE=static
	touch $@
.PHONY: exec-stamp

prefix ?= $(realpath $(dir $(shell which gnatls))/..)

install: install-static-lib install-relocatable-lib install-exec
.PHONY: install

install-static-lib: lib-static-stamp
	gprinstall				\
	  -P ews.gpr			\
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
	  -P ews.gpr			\
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
	  --prefix=$(prefix)			\
	  -XLIBRARY_TYPE=static			\
	  -f					\
	  -p
.PHONY: install-exec

clean:
	gnatclean -P make_htdocs.gpr
	gnatclean -P ews.gpr
	rm -rf .build-* bin include lib-*
	rm *-stamp
.PHONY: clean

# Distribution construction.

# Create the current date, in the form yyyymmdd.
RELEASE ?= $(shell date +%Y%m%d)

# Files to copy to the distribution
FILES = COPYING3 COPYING.RUNTIME README Makefile ews.gpr make_htdocs.gpr

# Distribution directory - eg ews-20130705
DISTDIR = ews-$(RELEASE)

# Subdirectories, each implements its own contribution.
SUBDIRS = doc src

dist: $(DISTDIR).tar.gz
.PHONY: dist

$(DISTDIR).tar.gz: $(DISTDIR)
	tar zcvf $@ $<

$(DISTDIR):
	-rm -rf $@
	mkdir $@
	cp $(FILES) $@/
	for s in $(SUBDIRS); do \
	  $(MAKE) DIST=$(PWD)/$@ -C $$s dist; \
	done
