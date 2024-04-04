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
STUB := $(NAME)_stub.h
DISTR := $(PKGNAME)-$(PKGVER).tar.gz

GHCVER := $(shell ghc --numeric-version)
GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)
DEPLIBS := $(MACHINE)-$(KERNEL)-ghc-$(GHCVER)
DISTDIR := dist
DISTV2DIR := dist-newstyle

all: $(DISTR)

$(DISTR): $(SRC)
	cabal install --lib --only-dependencies --package-env .
	sed -i 's/\(^package-id \)/--\1/' $(GHCENV)
	if ! command -v $(NHMTOOL) >/dev/null; then \
	  PATH=$$(dirname $$(cabal list-bin $(PKGDISTR))):$$PATH; \
	fi; \
	$(NHMTOOL) deps $(PKGNAME) >> $(GHCENV); \
	runhaskell --ghc-arg=-package=base \
	  --ghc-arg=-package=$(PKGDISTR) Setup.hs configure \
	  --package-db=clear --package-db=global \
	  $$(sed -n 's/^\(package-db\)\s\+/--\1=/p' $(GHCENV)) \
	  $$(sed -n 's/^package-id\s\+\(.*\)'` \
	    `'\(-\([0-9]\+\.\)*[0-9]\+\($$\|-.*\)\)/'` \
	    `'--dependency=\1=\1\2/p' \
	    $(GHCENV)) \
	  --prefix=$(PREFIX); \
	runhaskell --ghc-arg=-package=base \
	  --ghc-arg=-package=$(PKGDISTR) Setup.hs build \
	  --ghc-options="$(SRC) -o $(LIB) $(LINKRTS)"

install: $(DISTR)
	install -d $(PREFIX)
	tar xf $(DISTR) -C $(PREFIX) --no-same-owner

.PHONY: clean

clean:
	rm -rf $(DISTDIR) $(DISTV2DIR) $(DEPLIBS)
	rm -f $(GHCENV) $(STUB) $(NAME).hi $(NAME).o
	rm -f $(LIB)

clean-all: clean
	rm -f $(DISTR)

