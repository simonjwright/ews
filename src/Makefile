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

# Makefile for EWS source.

all::

SRC =						\
ews.ads						\
ews-dynamic.adb					\
ews-dynamic.ads					\
ews-http.adb					\
ews-http.ads					\
ews-http-ews_attachments_friend.adb		\
ews-http-ews_attachments_friend.ads		\
ews-make_htdocs.adb				\
ews-server.adb					\
ews-server.ads					\
ews-static.adb					\
ews-static.ads					\
ews-types.adb					\
ews-types.ads					\
input_sources-ews_attachments.adb		\
input_sources-ews_attachments.ads		\
HttpInteraction.js

dist:
	mkdir $(DIST)/src
	cp $(SRC) $(DIST)/src/
.PHONY: dist
