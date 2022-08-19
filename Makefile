# Copyright (C) 2013-2022, Simon Wright <simon@pushface.org>

# This file is part ofthe embedded web server EWS, hosted at Github
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

# Libraries

all:: lib-static-stamp lib-relocatable-stamp exec-stamp

lib-static-stamp: ews.gpr
	$(GPRBUILD) -p -P ews.gpr -XLIBRARY_TYPE=static
	touch $@

lib-relocatable-stamp: ews.gpr
	$(GPRBUILD) -p -P ews.gpr -XLIBRARY_TYPE=relocatable
	touch $@

# Executables

exec-stamp: lib-static-stamp
	$(GPRBUILD) -p -P generator/ews_generator.gpr -XLIBRARY_TYPE=static
	touch $@

# Demos

demo: exec-stamp
	$(MAKE) -w -C demonstrator $@
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

install-exec: exec-stamp			\
	gprinstall				\
	  -P generator/ews_generator.gpr	\
	  --install-name=ews			\
	  --prefix=$(prefix)			\
	  -XLIBRARY_TYPE=static			\
	  -f					\
	  -p
.PHONY: install-exec

clean:
	-gprclean -P generator/ews_generator.gpr
	-gprclean -P ews.gpr -XLIBRARY_TYPE=static
	-gprclean -P ews.gpr -XLIBRARY_TYPE=relocatable
	rm -f *-stamp
	$(MAKE) -w -C demonstrator clean
.PHONY: clean
