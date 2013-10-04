# -*- Makefile -*-

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

# Makefile for EWS demonstration.

all::
.PHONY: all

ifneq (,$(findstring mingw,$(shell gcc -dumpmachine)))
  EXE = .exe
endif

demo: ews_demo$(EXE)
.PHONY: demo

EWS_GENERATED = ews_htdocs.ads ews_htdocs.adb

$(EWS_GENERATED): index.html ajax.html ajax.js HttpInteraction.js favicon.ico
	../bin/ews-make_htdocs

ews_demo$(EXE): ews_demo.gpr ews_demo.adb $(EWS_GENERATED)
	gnatmake -p -P ews_demo

dist:
.PHONY: dist

clean:
	-rm -rf .build
	-rm ews_demo$(EXE) ews_htdocs.*
.PHONY: clean
