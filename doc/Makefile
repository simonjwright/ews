# $Id$
#
# Makefile for EWS documentation.

USER = simonjwright
PRJ = embed-web-srvr
FILES = index.html

all::

upload: $(FILES)
	rsync -av $(FILES) $(USER),$(PRJ)@web.sourceforge.net:htdocs/
