# Build and install dependencies in local GHC environment with v2-install,
# then build the library and create a tarball with all dependent libraries

NAME := ngx_distribution_test
PKGNAME := ngx-distribution-test
PKGVER := 0.1.0.0

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

all: $(DISTR)

$(DISTR): $(SRC)
	cabal v2-install --lib --only-dependencies --package-env .
	cabal v2-build $(PKGDISTR)
	sed -i 's/\(^package-id \)/--\1/' $(GHCENV)
	runhaskell --ghc-arg=-package=base \
		--ghc-arg=-package=$(PKGDISTR) Setup.hs configure \
		--package-db=clear --package-db=global \
		$$(sed -n 's/^\(package-db\)\s\+/--\1=/p' $(GHCENV)) \
		--prefix=$(PREFIX)
	. ../cabal-plan-direct-deps.sh >> $(GHCENV)
	runhaskell --ghc-arg=-package=base \
		--ghc-arg=-package=$(PKGDISTR) Setup.hs build \
		--ghc-options="$(SRC) -o $(LIB) -threaded"

.PHONY: clean

clean:
	rm -rf $(DISTDIR) $(DISTV2DIR) $(DEPLIBS)
	rm -f $(GHCENV) $(STUB)
	rm -f $(LIB)

clean-all: clean
	rm -f $(DISTR)

