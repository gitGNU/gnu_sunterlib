s48-interfaces := $(shell find s48 -name interfaces.scm)
s48-packages := $(shell find s48 -name packages.scm)
scsh-interfaces := $(shell find scsh -name interfaces.scm)
scsh-packages := $(shell find scsh -name packages.scm)

targets = s48-interfaces.scm s48-packages.scm interfaces.scm packages.scm

all : $(targets)

s48-interfaces.scm : $(s48-interfaces)
	cat $(s48-interfaces) > s48-interfaces.scm

s48-packages.scm : $(s48-packages)
	cat $(s48-packages) > s48-packages.scm

interfaces.scm : $(s48-interfaces) $(scsh-interfaces)
	cat $(s48-interfaces) $(scsh-interfaces) > interfaces.scm

packages.scm : $(s48-packages) $(scsh-packages)
	cat $(s48-packages) $(scsh-packages) > packages.scm

.PHONY : clean
clean :
	-rm -f $(targets)
