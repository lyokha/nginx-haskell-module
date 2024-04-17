NAME := upconf
PKGNAME := $(subst _,-,$(NAME))
PKGVER := 0.1.0.0

PREFIX := /var/lib/nginx
MACHINE := $(shell uname -m)
KERNEL := $(shell uname -s | tr A-Z a-z)

PKGDISTR := ngx-export-distribution
NHMTOOL := nhm-tool

SRC := $(NAME).hs
LIB := $(NAME).so
DISTR := $(PKGNAME)-$(PKGVER).tar.gz

OBJS := $(SRC:.hs=.o)
HIOBJS := $(SRC:.hs=.hi)
DYNOBJS := $(SRC:.hs=.dyn_o)
DYNHIOBJS := $(SRC:.hs=.dyn_hi)
STUBS := $(SRC:.hs=_stub.h)

GHC := ghc
CABAL := cabal
GHCVER := $(shell $(GHC) --numeric-version)
GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)
DEPLIBS := $(MACHINE)-$(KERNEL)-ghc-$(GHCVER)
BUILDDIR := dist-nhm
SETUPCONFIG := $(BUILDDIR)/setup-config

.PHONY: all env config install clean clean-all

all: $(DISTR)

env: $(GHCENV)

config: $(SETUPCONFIG)

$(GHCENV): cabal.project $(PKGNAME).cabal
	$(CABAL) install --builddir="$(BUILDDIR)" --lib --only-dependencies \
	  --package-env .
	sed -i 's/\(^package-id \)/--\1/' $(GHCENV)
	if test "$(NHMTOOL)" = nhm-tool && ! command -v nhm-tool >/dev/null; \
	then \
	  PATH=$$(dirname $$($(CABAL) list-bin $(PKGDISTR) \
	    --builddir="$(BUILDDIR)")):$$PATH; \
	fi; \
	$(NHMTOOL) deps $(PKGNAME) -d "$(BUILDDIR)" >> $(GHCENV)

$(SETUPCONFIG): $(GHCENV)
	if test "$(NHMTOOL)" = nhm-tool && ! command -v nhm-tool >/dev/null; \
	then \
	  PATH=$$(dirname $$($(CABAL) list-bin $(PKGDISTR) \
	    --builddir="$(BUILDDIR)")):$$PATH; \
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
	  --prefix=$(PREFIX)

$(DISTR): $(SETUPCONFIG) $(SRC)
	if test "$(NHMTOOL)" = nhm-tool && ! command -v nhm-tool >/dev/null; \
	then \
	  PATH=$$(dirname $$($(CABAL) list-bin $(PKGDISTR) \
	    --builddir="$(BUILDDIR)")):$$PATH; \
	fi; \
	runhaskell --ghc-arg=-package=base \
	  --ghc-arg=-package=$(PKGDISTR) Setup.hs build \
	  --builddir="$(BUILDDIR)" \
	  --ghc-options="$(SRC) -o $(LIB) $(LINKRTS)"

install: $(DISTR)
	install -d $(PREFIX)
	tar xf $(DISTR) -C $(PREFIX) --no-same-owner

clean:
	rm -rf $(DEPLIBS)
	rm -f $(OBJS) $(HIOBJS) $(DYNOBJS) $(DYNHIOBJS) $(STUBS)
	rm -f $(LIB)

clean-all: clean
	rm -rf $(BUILDDIR)
	rm -f $(GHCENV) $(DISTR)

