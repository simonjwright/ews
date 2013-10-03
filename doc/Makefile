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

all::

DOCS =						\
../doc/index.html				\
../doc/ajax.html				\
../doc/ajax.js					\
../doc/favicon.ico

dist:
	mkdir $(DIST)/doc
	cp $(DOCS) $(DIST)/doc/
.PHONY: dist

USER = simonjwright
PRJ = embed-web-srvr
FILES = index.html

upload: $(FILES)
	rsync -av $(FILES) $(USER),$(PRJ)@web.sourceforge.net:htdocs/
