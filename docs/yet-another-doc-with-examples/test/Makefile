NAME := test
PKGNAME := $(subst _,-,$(NAME))
PKGVER := 0.1.0.0
CPLUG := $(NAME)_c_plugin

PREFIX := /var/lib/nginx
MACHINE := $(shell uname -m)
KERNEL := $(shell uname -s | tr A-Z a-z)

PKGDISTR := ngx-export-distribution
NHMTOOL := nhm-tool

SRC := $(NAME).hs
LIB := $(NAME).so
STUB := $(NAME)_stub.h
DISTR := $(PKGNAME)-$(PKGVER).tar.gz

GHCVER := $(shell ghc --numeric-version)
GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)
DEPLIBS := $(MACHINE)-$(KERNEL)-ghc-$(GHCVER)
BUILDDIR := dist-nhm

PKGNGXEXPORT := ngx-export

INPLACE := $(shell grep -q '^packages:.*\(\.\./\)\{3\}haskell/$(PKGNGXEXPORT)' \
                       cabal.project && echo 1 || echo 0)

NGX_HOME := .

.PHONY: all env clean

all: $(DISTR)

env: $(GHCENV)

$(GHCENV): cabal.project $(PKGNAME).cabal
	cabal install --builddir="$(BUILDDIR)" --lib --only-dependencies \
	  --package-env .
ifeq ($(INPLACE),1)
	cabal build --builddir="$(BUILDDIR)" $(PKGNGXEXPORT)
	echo "package-db $$(pwd)/$(BUILDDIR)/packagedb/ghc-$(GHCVER)" >> \
	  $(GHCENV)
endif
	sed -i 's/\(^package-id \)/--\1/' $(GHCENV)
	if test "$(NHMTOOL)" = nhm-tool && ! command -v nhm-tool >/dev/null; \
	then \
	  PATH=$$(dirname \
	    $$(cabal list-bin $(PKGDISTR) --builddir="$(BUILDDIR)")):$$PATH; \
	fi; \
	$(NHMTOOL) deps $(PKGNAME) -d "$(BUILDDIR)" >> $(GHCENV)

$(DISTR): $(GHCENV) $(SRC) $(CPLUG).o
	if test "$(NHMTOOL)" = nhm-tool && ! command -v nhm-tool >/dev/null; \
	then \
	  PATH=$$(dirname \
	    $$(cabal list-bin $(PKGDISTR) --builddir="$(BUILDDIR)")):$$PATH; \
	fi; \
	runhaskell --ghc-arg=-package=base \
	  --ghc-arg=-package=$(PKGDISTR) Setup.hs configure \
	  --builddir="$(BUILDDIR)" \
	  --package-db=clear --package-db=global \
	  $$(sed -n 's/^\(package-db\)\s\+/--\1=/p' $(GHCENV)) \
	  $$(sed -n 's/^package-id\s\+\(.*\)'` \
	    `'\(-\([0-9]\+\.\)*[0-9]\+\($$\|-.*\)\)/'` \
	    `'--dependency=\1=\1\2/p' \
	    $(GHCENV)) \
	  --prefix=$(PREFIX); \
	runhaskell --ghc-arg=-package=base \
	  --ghc-arg=-package=$(PKGDISTR) Setup.hs build \
	  --builddir="$(BUILDDIR)" \
	  --ghc-options="$(CPLUG).o $(SRC) -o $(LIB) -ddump-splices $(LINKRTS)"

$(CPLUG).o: $(CPLUG).c
	gcc -O2 -fPIC -c $(CPLUG).c -o $(CPLUG).o \
	  -I$(NGX_HOME)/src/core -I$(NGX_HOME)/src/http \
	  -I$(NGX_HOME)/src/http/modules \
	  -I$(NGX_HOME)/src/event -I$(NGX_HOME)/src/event/modules \
	  -I$(NGX_HOME)/src/os/unix -I$(NGX_HOME)/objs

install: $(DISTR)
	install -d $(PREFIX)
	tar xf $(DISTR) -C $(PREFIX) --no-same-owner

clean:
	rm -rf $(BUILDDIR) $(DEPLIBS)
	rm -f $(STUB) $(NAME).hi $(NAME).o $(CPLUG).o
	rm -f $(LIB)

clean-all: clean
	rm -f $(GHCENV) $(DISTR)

