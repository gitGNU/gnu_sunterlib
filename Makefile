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

s48-interfaces.scm : $(s48-interfaces)
	cat $(s48-interfaces) > s48-interfaces.scm

s48-packages.scm : $(s48-packages)
	build/xpackages.scm s48-packages.scm $(s48-packages)

interfaces.scm : $(s48-interfaces) $(scsh-interfaces)
	cat $(s48-interfaces) $(scsh-interfaces) > interfaces.scm

packages.scm : $(s48-packages) $(scsh-packages)
	build/xpackages.scm packages.scm $(s48-packages) $(scsh-packages)

.PHONY : clean
clean :
	-rm -f $(targets)
