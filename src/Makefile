#  Copyright (C) Simon Wright <simon@pushface.org>

#  This package is free software; you can redistribute it and/or
#  modify it under terms of the GNU General Public License as
#  published by the Free Software Foundation; either version 2, or
#  (at your option) any later version. This package is distributed in
#  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
#  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#  PARTICULAR PURPOSE. See the GNU General Public License for more
#  details. You should have received a copy of the GNU General Public
#  License distributed with this package; see file COPYING.  If not,
#  write to the Free Software Foundation, 59 Temple Place - Suite
#  330, Boston, MA 02111-1307, USA.

# $Id$

# No default.
all::
	@echo "valid make targets are:"
	@echo "   ews-server-test (the demo)"
	@echo "   ews-htdocs.adb"
	@echo "   ews-make_htdocs"
	@echo "   dist [SUBRELEASE=r (d=cvs)]"

ews-server-test: ews-htdocs.adb .build
	gnatmake -PEWS -gnat95 ews-server-test
clean::
	rm -f ews-server-test

ews-htdocs.adb: ews-make_htdocs force
	cp HttpInteraction.js ../doc/
	(cd ../doc && ../src/ews-make_htdocs >../src/$@)
clean::
	rm -f ews-htdocs.adb ../doc/HttpInteraction.js

ews-make_htdocs: ews-make_htdocs.adb .build
	gnatmake -PEWS -gnat95 ews-make_htdocs
clean::
	rm -f ews-make_htdocs

.build:
	mkdir .build
clean::
	rm -rf .build

############################
# Distribution construction

# Create the current date, in the form yyyymmdd. This works fine in Linux.
SUBRELEASE ?= cvs
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

DOCS = \
../doc/index.html \
../doc/ajax.html \
../doc/ajax.js

SRC = \
EWS.gpr \
ews.ads \
ews-dynamic.adb \
ews-dynamic.ads \
ews-htdocs.ads \
ews-http.adb \
ews-http.ads \
ews-http-ews_attachments_friend.adb \
ews-http-ews_attachments_friend.ads \
ews-make_htdocs.adb \
ews-server.adb \
ews-server.ads \
ews-server-test.adb \
ews-static.adb \
ews-static.ads \
ews-types.adb \
ews-types.ads \
input_sources-ews_attachments.adb \
input_sources-ews_attachments.ads \
HttpInteraction.js

DISTRIBUTION_FILES = \
ews-$(DATE).tgz \
ews-$(DATE).zip

dist: ews-$(DATE) $(DISTRIBUTION_FILES) COPYING $(DOCS)
	-@rm -rf dist
	mkdir -p dist/download
	cp -p COPYING $(DOCS) dist/
	cp $(DISTRIBUTION_FILES) dist/download/

ews-$(DATE): $(DOCS) $(SRC) Makefile-dist force
	-rm -rf $@
	mkdir -p $@
	cp -p $(SRC) $@/
	cp -p Makefile-dist $@/Makefile
	mkdir $@/html
	cp -p $(DOCS) $@/html/

ews-$(DATE).tgz: ews-$(DATE)
	-rm $@
	tar zcvf $@ $</

ews-$(DATE).zip: ews-$(DATE)
	-rm $@
	zip -r $@ $</*

.PHONY: force
