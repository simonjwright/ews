# -*- Makefile -*-

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

ifeq ($(OS),Windows_NT)
  DEL   = del
  TOUCH = $(MAKE) --touch
else
  DEL   = rm -f
  TOUCH = touch
endif

# Subdirectories, each implements its own contribution.
SUBDIRS = doc src

# Libraries

all:: lib-static-stamp lib-relocatable-stamp exec-stamp

lib-static-stamp: ews.gpr
	$(GPRBUILD) -p -P ews.gpr -XLIBRARY_TYPE=static
	$(TOUCH) $@

lib-relocatable-stamp: ews.gpr
	$(GPRBUILD) -p -P ews.gpr -XLIBRARY_TYPE=relocatable
	$(TOUCH) $@

# Executables

exec-stamp: lib-static-stamp make_htdocs.gpr
	$(GPRBUILD) -p -P make_htdocs.gpr -XLIBRARY_TYPE=static
	$(TOUCH) $@

# Demos

demo:
	for s in $(SUBDIRS); do \
	  $(MAKE) -w -C $$s $@; \
	done
.PHONY: demo

# Installation

ifeq ($(OS),Windows_NT)
# Don't know the Windows equivalent of 'which'
ifeq ($(prefix),)
install::
	$(error Cant "make install" on Windows unless "prefix" is set)
endif
else
prefix ?= $(realpath $(dir $(shell which gnatls))/..)
endif

install:: install-static-lib install-relocatable-lib install-exec
.PHONY: install

install-static-lib: lib-static-stamp
	gprinstall				\
	  -P ews.gpr				\
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
	-gnatclean -P make_htdocs.gpr
	-gnatclean -P ews.gpr -XLIBRARY_TYPE=static
	-gnatclean -P ews.gpr -XLIBRARY_TYPE=relocatable
	$(DEL) *-stamp
	for s in $(SUBDIRS); do \
	  $(MAKE) -w -C $$s $@; \
	done
.PHONY: clean

# Distribution.

dist:
	$(error Can only build distribution from an Hg checkout.)
.PHONY: dist
