NAME := test
PKGNAME := $(subst _,-,$(NAME))
PKGVER := 0.1.0.0
CPLUG := $(NAME)_c_plugin

PREFIX := /var/lib/nginx
MACHINE := $(shell uname -m)
KERNEL := $(shell uname -s | tr A-Z a-z)

PKGDISTR := ngx-export-distribution

SRC := $(NAME).hs
LIB := $(NAME).so
STUB := $(NAME)_stub.h
DISTR := $(PKGNAME)-$(PKGVER).tar.gz

GHCVER := $(shell ghc --numeric-version)
GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)
DEPLIBS := $(MACHINE)-$(KERNEL)-ghc-$(GHCVER)
DISTDIR := dist
DISTV2DIR := dist-newstyle

NGX_HOME := .

all: $(DISTR)

$(DISTR): $(SRC) $(CPLUG).o
	cabal install --lib --only-dependencies --package-env .
	cabal build $(PKGDISTR)
	sed -i 's/\(^package-id \)/--\1/' $(GHCENV)
	runhaskell --ghc-arg=-package=base \
	  --ghc-arg=-package=$(PKGDISTR) Setup.hs configure \
	  --package-db=clear --package-db=global \
	  $$(sed -n 's/^\(package-db\)\s\+/--\1=/p' $(GHCENV)) \
	  --prefix=$(PREFIX)
	nhm-tool plan >> $(GHCENV)
	runhaskell --ghc-arg=-package=base \
	  --ghc-arg=-package=$(PKGDISTR) Setup.hs build \
	  --ghc-options="$(CPLUG).o $(SRC) -o $(LIB) $(LINKRTS)"

$(CPLUG).o: $(CPLUG).c
	gcc -O2 -fPIC -c $(CPLUG).c -o $(CPLUG).o \
	  -I$(NGX_HOME)/src/core -I$(NGX_HOME)/src/http \
	  -I$(NGX_HOME)/src/http/modules \
	  -I$(NGX_HOME)/src/event -I$(NGX_HOME)/src/event/modules \
	  -I$(NGX_HOME)/src/os/unix -I$(NGX_HOME)/objs

install: $(DISTR)
	install -d $(PREFIX)
	tar xf $(DISTR) -C $(PREFIX) --no-same-owner

.PHONY: clean

clean:
	rm -rf $(DISTDIR) $(DISTV2DIR) $(DEPLIBS)
	rm -f $(GHCENV) $(STUB) $(NAME).hi $(NAME).o $(CPLUG).o
	rm -f $(LIB)

clean-all: clean
	rm -f $(DISTR)

