SHELL = /bin/sh

.SUFFIXES:

major-version = 0
minor-version = 6
version := $(major-version).$(minor-version)

prefix = /tmp/sunterlib

s48-authors := $(shell find s48 \
                  -maxdepth 2 -mindepth 2 \
                  -name AUTHORS)
s48-blurbs := $(shell find s48 \
                -maxdepth 2 -mindepth 2 \
                -name BLURB)

scsh-authors := $(shell find scsh \
                  -maxdepth 2 -mindepth 2 \
                  -name AUTHORS)
scsh-blurbs := $(shell find scsh \
                 -maxdepth 2 -mindepth 2 \
                 -name BLURB)

targets := DETAILS COPYING pkg-def.scm

.PHONY: all
all : $(targets)

DETAILS : $(s48-authors) $(s48-blurbs) $(scsh-authors) $(scsh-blurbs) \
          build/details.scm build/dirs.scm build/header.scm
	build/details.scm

COPYING : $(s48-authors) $(scsh-authors) \
          build/copying.scm build/common.scm build/header.scm build/dirs.scm
	build/copying.scm

pkg-def.scm : $(s48-authors) $(scsh-authors) \
	      build/make-pkg-def.scm build/common.scm build/header.scm \
              build/dirs.scm
	build/make-pkg-def.scm $(major-version) $(minor-version)

.PHONY : install uninstall
install : $(targets)
	scsh-install-pkg --prefix $(prefix)

.PHONY : dist
dist : $(targets)
	mkdir sunterlib-$(version)
	cp pkg-def.scm COPYING DETAILS INSTALL NEWS README README.contrib install-pkg \
           pkg-def.scm sunterlib-$(version)/
	cp -r s48 sunterlib-$(version)/s48
	cp -r scsh sunterlib-$(version)/scsh
	find sunterlib-$(version)/ -name CVS | xargs rm -rf
	find sunterlib-$(version)/ -name "*~" | xargs rm -f
	tar -czf sunterlib-$(version).tar.gz sunterlib-$(version)
	rm -rf sunterlib-$(version)

.PHONY : clean distclean
clean :
	-rm -f $(targets)

distclean : clean
	-rm -rf sunterlib-$(version) sunterlib-$(version).tar.gz
