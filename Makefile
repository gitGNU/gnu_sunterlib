SHELL = /bin/sh

prefix = /usr/local
libdir = $(prefix)/lib
docdir = $(prefix)/share/doc
pkglibdir = $(libdir)/sunterlib
pkgdocdir = $(docdir)/sunterlib

INSTALL = /usr/bin/install
INSTALL_DATA = $(INSTALL) -m 644

.SUFFIXES:

version := 0.3

s48-interfaces := $(shell find s48 \
                    -maxdepth 2 -mindepth 2 \
                    -name interfaces.scm)
s48-packages := $(shell find s48 \
                  -maxdepth 2 -mindepth 2 \
                  -name packages.scm)
s48-srcs := $(shell find s48 \
              -mindepth 2 \
              ! -name interfaces.scm \
              ! -name packages.scm \
              -name "*.scm")
s48-docs := $(shell find s48 \
              -mindepth 2 \
              -name README)
s48-authors := $(shell find s48 \
                  -maxdepth 2 -mindepth 2 \
                  -name AUTHORS)
s48-blurbs := $(shell find s48 \
                -maxdepth 2 -mindepth 2 \
                -name BLURB)

scsh-interfaces := $(shell find scsh \
                     -maxdepth 2 -mindepth 2 \
                     -name interfaces.scm)
scsh-packages := $(shell find scsh \
                     -maxdepth 2 -mindepth 2 \
                     -name packages.scm)
scsh-srcs := $(shell find scsh \
               -mindepth 2 \
               ! -name interfaces.scm \
               ! -name packages.scm \
               -name "*.scm")
scsh-docs := $(shell find scsh \
              -mindepth 2 \
              -name README)
scsh-authors := $(shell find scsh \
                  -maxdepth 2 -mindepth 2 \
                  -name AUTHORS)
scsh-blurbs := $(shell find scsh \
                 -maxdepth 2 -mindepth 2 \
                 -name BLURB)

s48-targets := s48-interfaces.scm s48-packages.scm
scsh-targets := interfaces.scm packages.scm
targets := $(s48-targets) $(scsh-targets) DETAILS

.PHONY: all s48 scsh
all : s48 scsh DETAILS
s48 : $(s48-targets)
scsh : $(scsh-targets)

s48-interfaces.scm : $(s48-interfaces) build/header.scm
	cat build/header.scm > s48-interfaces.scm
	for interface in $(s48-interfaces) ; \
	  do \
	    cat $${interface} >> s48-interfaces.scm ; \
          done

s48-packages.scm : $(s48-packages) build/header.scm
	build/xpackages.scm s48-packages.scm build/header.scm $(s48-packages)

interfaces.scm : $(s48-interfaces) $(scsh-interfaces) build/header.scm
	cat build/header.scm > interfaces.scm
	for interface in $(s48-interfaces) $(scsh-interfaces) ; \
	  do \
	    cat $${interface} >> interfaces.scm ; \
	  done

packages.scm : $(s48-packages) $(scsh-packages) build/header.scm
	build/xpackages.scm packages.scm build/header.scm $(s48-packages) $(scsh-packages)

DETAILS : $(s48-authors) $(s48-blurbs) $(scsh-authors) $(scsh-blurbs)
	build/details.scm

.PHONY : install uninstall
install : s48 scsh DETAILS
	$(INSTALL) -d $(pkglibdir)
	$(INSTALL_DATA) s48-interfaces.scm s48-packages.scm $(pkglibdir)
	$(INSTALL_DATA) interfaces.scm packages.scm $(pkglibdir)
	$(INSTALL) -d $(pkgdocdir)
	$(INSTALL_DATA) README $(pkgdocdir)
	$(INSTALL_DATA) DETAILS $(pkgdocdir)
	for s48src in $(s48-srcs); \
	  do \
            $(INSTALL) -d $(pkglibdir)/`dirname $${s48src}`; \
            $(INSTALL_DATA) $${s48src} $(pkglibdir)/$${s48src}; \
          done
	for s48doc in $(s48-docs); \
	  do \
            $(INSTALL) -d $(pkgdocdir)/`dirname $${s48doc}`; \
            $(INSTALL_DATA) $${s48doc} $(pkgdocdir)/$${s48doc}; \
          done
	for scshsrc in $(scsh-srcs); \
	  do \
            $(INSTALL) -d $(pkglibdir)/`dirname $${scshsrc}`; \
            $(INSTALL_DATA) $${scshsrc} $(pkglibdir)/$${scshsrc}; \
          done
	for scshdoc in $(scsh-docs); \
	  do \
            $(INSTALL) -d $(pkgdocdir)/`dirname $${scshdoc}`; \
            $(INSTALL_DATA) $${scshdoc} $(pkgdocdir)/$${scshdoc}; \
          done

uninstall :
	-rm -rf $(pkglibdir) $(pkgdocdir)


.PHONY : dist
dist :
	mkdir sunterlib-$(version)
	cp COPYING INSTALL Makefile NEWS README README.admin README.contrib sunterlib-$(version)/
	cp -r build sunterlib-$(version)/build
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
