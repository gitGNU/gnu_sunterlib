SHELL = /bin/sh
.SUFFIXES:

version := 0.1

s48-interfaces := $(shell find s48 \
                    -maxdepth 2 -mindepth 2 \
                    -name interfaces.scm)
s48-packages := $(shell find s48 \
                  -maxdepth 2 -mindepth 2 \
                  -name packages.scm)
scsh-interfaces := $(shell find scsh \
                     -maxdepth 2 -mindepth 2 \
                     -name interfaces.scm)
scsh-packages := $(shell find scsh \
                     -maxdepth 2 -mindepth 2 \
                     -name packages.scm)

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

.PHONY : dist
dist :
	mkdir sunterlib-$(version)
	cp COPYING Makefile NEWS README sunterlib-$(version)/
	cp -r build sunterlib-$(version)/build
	cp -r s48 sunterlib-$(version)/s48
	cp -r scsh sunterlib-$(version)/scsh
	find sunterlib-0.1/ -name CVS | xargs rm -rf
	find sunterlib-0.1/ -name "*~" | xargs rm -f
	tar -czf sunterlib-$(version).tar.gz sunterlib-$(version)
	rm -rf sunterlib-$(version)

.PHONY : clean distclean
clean :
	-rm -f $(targets)

distclean : clean
	-rm -rf sunterlib-$(version) sunterlib-$(version).tar.gz
