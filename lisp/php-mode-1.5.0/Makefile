#!/usr/bin/make -f

### php-mode.make -- Makefile for PHP mode for Emcs

## Copyright (C) 2008  Aaron S. Hawley

## Author: Aaron S. Hawley <ashawley at users sourceforge net>
## $Revision: 90 $
## $Date: 2008-11-04 11:51:51 -0500 (Tue, 04 Nov 2008) $
## X-URL: http://php-mode.sourceforge.net/

## This file is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published
## by the Free Software Foundation; either version 3, or (at your
## option) any later version.

## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs; see the file COPYING.  If not, write to the
## Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301, USA.

### Commentary:

## Build a release source distribution of PHP mode:

## $ make dist

## Build a PDF version of the manual:

## $ make php-mode.pdf

## Build a Postscript version of the manual:

## $ make php-mode.ps

## Build a device independent (DVI) version of the manual:

## $ make php-mode.ps

## Build an HTML version (monolothic and multi-page) of the manual:

## $ make php-mode.html

## Build an HTML monolothic and multi-page versions of the manual:

## $ make php-mode.html

## Build a GNU Info version of the manual:

## $ make php-mode.info

## Build a Gzipped version of any file by adding the .gz suffix:

## $ make php-mode.info.gz

## Build the manual in all the formats:

## $ make

### History:

## Written on January 3, 2008 in South Burlington, Vermont, USA.

### Code:

package = php-mode
version = 1.5.0
release = $(package)-$(version)

lisp_files = *.el
texi_files = *.texi
#info_files = *.info
#html_files = *.html

CCMODE_MANUAL = http://cc-mode.sourceforge.net/html-manual/
EMACS_MANUAL = http://emacswiki.org/cgi-bin/info-ref?find=

CP ?= cp -p
GZIP ?= gzip -f
TAR ?= tar
ZIP ?= zip

dist: $(release).tar.gz $(release).zip
	$(RM) -r $(release)

manuals: php-mode.info.gz php-mode.pdf php-mode.ps.gz php-mode.html

$(release): $(lisp_files) $(texi_files) Makefile ChangeLog 
	mkdir $@ \
	 && $(CP) $^ $@

$(release).tar: $(release)
	 $(TAR) cf $@ $<

$(release).zip: $(release)
	 $(ZIP) -9 -r $@ $<

%.gz: %
	$(GZIP) $<

%.html: %.texi
	makeinfo --html --output $@ --no-split --no-headers $<
	makeinfo --html --output html-manual $<
	for f in html-manual/*.html $@; do \
	  $(CP) $${f} $${f}~; \
	  perl -pe 'while (/emacs\.html#[^"-]*-/){s/(emacs\.html#[^"-]*)-/\1 /g;}' $${f}~ \
	   | sed -e 's!ccmode\.html!${CCMODE_MANUAL}!g' \
	         -e 's!\.\./ccmode/\([^#]*\)!${CCMODE_MANUAL}\1!g' \
	         -e 's!emacs\.html\#!${EMACS_MANUAL}!g' \
	         -e 's!\.\./emacs/\([^#]*\)!${EMACS_MANUAL}\1!g' > $${f}; \
	done

%.info: %.texi
	makeinfo $<

%.txt: %.texi
	makeinfo --plaintext --output $@ --no-split --no-headers $<

%.pdf: %.texi
	texi2pdf $<

%.dvi: %.texi
	texi2dvi $<

%.ps: %.dvi
	dvips -o $@ $<
