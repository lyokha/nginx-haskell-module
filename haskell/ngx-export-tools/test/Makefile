NAME := test_tools
PKGNAME := $(subst _,-,$(NAME))
PKGVER := 0.1.0.0

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

INPLACE := $(shell grep -q '^packages:.*\.\./ngx-export-tools' \
                       cabal.project && echo 1 || echo 0)

.PHONY: all env clean

all: $(DISTR)

env: $(GHCENV)

$(GHCENV): cabal.project $(PKGNAME).cabal
	cabal install --builddir="$(BUILDDIR)" --lib --only-dependencies \
	  --package-env .
ifeq ($(INPLACE),1)
	cabal build --builddir="$(BUILDDIR)" ngx-export-tools
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

$(DISTR): $(SRC)
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
	  --ghc-options="$(SRC) -o $(LIB) -ddump-splices $(LINKRTS)"

install: $(DISTR)
	install -d $(PREFIX)
	tar xf $(DISTR) -C $(PREFIX) --no-same-owner

clean:
	rm -rf $(BUILDDIR) $(DEPLIBS)
	rm -f $(GHCENV) $(STUB) $(NAME).hi $(NAME).o
	rm -f $(LIB)

clean-all: clean
	rm -f $(DISTR)

