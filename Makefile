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
              -not -name interfaces.scm \
              -not -name packages.scm \
              -name "*.scm")
s48-docs := $(shell find s48 \
              -mindepth 2 \
              -name README)

scsh-interfaces := $(shell find scsh \
                     -maxdepth 2 -mindepth 2 \
                     -name interfaces.scm)
scsh-packages := $(shell find scsh \
                     -maxdepth 2 -mindepth 2 \
                     -name packages.scm)
scsh-srcs := $(shell find scsh \
               -mindepth 2 \
               -not -name interfaces.scm \
               -not -name packages.scm \
               -name "*.scm")
scsh-docs := $(shell find scsh \
              -mindepth 2 \
              -name README)

s48-targets := s48-interfaces.scm s48-packages.scm
scsh-targets := interfaces.scm packages.scm
targets := $(s48-targets) $(scsh-targets)

.PHONY: all s48 scsh
all : s48 scsh
s48 : $(s48-targets)
scsh : $(scsh-targets)

s48-interfaces.scm : $(s48-interfaces) build/header.scm
	cat build/header.scm $(s48-interfaces) > s48-interfaces.scm

s48-packages.scm : $(s48-packages) build/header.scm
	build/xpackages.scm s48-packages.scm build/header.scm $(s48-packages)

interfaces.scm : $(s48-interfaces) $(scsh-interfaces) build/header.scm
	cat build/header.scm $(s48-interfaces) $(scsh-interfaces) > interfaces.scm

packages.scm : $(s48-packages) $(scsh-packages) build/header.scm
	build/xpackages.scm packages.scm build/header.scm $(s48-packages) $(scsh-packages)

.PHONY : install uninstall
install : s48 scsh
	$(INSTALL) -d $(pkglibdir)
	$(INSTALL_DATA) s48-interfaces.scm s48-packages.scm $(pkglibdir)
	$(INSTALL_DATA) interfaces.scm packages.scm $(pkglibdir)
	$(foreach s48-src, \
                  $(s48-srcs), \
                  $(INSTALL) -d $(pkglibdir)/$(dir $(s48-src)); \
                  $(INSTALL_DATA) $(s48-src) $(pkglibdir)/$(s48-src);)
	$(foreach s48-doc, \
                  $(s48-docs), \
                  $(INSTALL) -d $(pkgdocdir)/$(dir $(s48-doc)); \
                  $(INSTALL_DATA) $(s48-doc) $(pkgdocdir)/$(s48-doc);)
	$(foreach scsh-src, \
                  $(scsh-srcs), \
                  $(INSTALL) -d $(pkglibdir)/$(dir $(scsh-src)); \
                  $(INSTALL_DATA) $(scsh-src) $(pkglibdir)/$(scsh-src);)
	$(foreach scsh-doc, \
                  $(scsh-docs), \
                  $(INSTALL) -d $(pkgdocdir)/$(dir $(scsh-doc)); \
                  $(INSTALL_DATA) $(scsh-doc) $(pkgdocdir)/$(scsh-doc);)

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
