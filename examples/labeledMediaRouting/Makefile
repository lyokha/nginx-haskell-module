NAME := lmr
PKGNAME := $(subst _,-,$(NAME))
PKGVER := 0.1.0.0

TESTDIR := test
BENCH := $(TESTDIR)/$(NAME)-bench

PREFIX := /var/lib/nginx
MACHINE := $(shell uname -m)
KERNEL := $(shell uname -s | tr A-Z a-z)

PKGDISTR := ngx-export-distribution
NHMTOOL := nhm-tool

SRC := $(NAME).hs
LIB := $(NAME).so
DISTR := $(PKGNAME)-$(PKGVER).tar.gz

OBJS := $(SRC:.hs=.o) $(BENCH).o $(TESTDIR)/lmr-objgen.o
HIOBJS := $(SRC:.hs=.hi) $(BENCH).hi $(TESTDIR)/lmr-objgen.hi
DYNOBJS := $(SRC:.hs=.dyn_o)
DYNHIOBJS := $(SRC:.hs=.dyn_hi)
STUBS := $(SRC:.hs=_stub.h)

GHC := ghc
CABAL := cabal
GHCVER := $(shell $(GHC) --numeric-version)
CABALVER := $(shell $(CABAL) --numeric-version)
GHCPUID := $(shell \
    if printf '3.12\n$(CABALVER)' | sort -VC && \
       printf '9.10\n$(GHCVER)' | sort -VC; \
    then ghcpuid=`$(GHC) --info | sed -n \
             's/^.*\x28"Project Unit Id","\(.*\)"\x29.*$$/\1/ip'`; \
        if test -z "$$ghcpuid"; \
        then echo "ghc-$(GHCVER)"; \
        else echo "$$ghcpuid"; \
        fi; \
    else echo "ghc-$(GHCVER)"; \
    fi)
GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)
GHCENVLNK := .ghc.environment.lnk
DEPLIBS := $(MACHINE)-$(KERNEL)-$(GHCPUID)
BUILDDIR := dist-nhm
SETUPCONFIG := $(BUILDDIR)/setup-config

.PHONY: all env config install clean clean-all bench

all: env $(DISTR)

env: $(GHCENVLNK) $(TESTDIR)/$(GHCENV) $(TESTDIR)/LabeledMediaRouter.hs

config: $(SETUPCONFIG)

bench: $(BENCH)

$(GHCENVLNK): cabal.project $(PKGNAME).cabal
	rm -f $(GHCENVLNK)
	$(CABAL) install --builddir="$(BUILDDIR)" --lib --only-dependencies \
	  --package-env .
	$(CABAL) build --builddir="$(BUILDDIR)" ngx-export
	echo "package-db $$(pwd)/$(BUILDDIR)/packagedb/ghc-$(GHCVER)" >> \
	  $(GHCENV)
	sed -i 's/\(^package-id \)/--\1/' $(GHCENV)
	if test "$(NHMTOOL)" = nhm-tool && ! command -v nhm-tool >/dev/null; \
	then \
	  PATH=$$(dirname $$($(CABAL) list-bin $(PKGDISTR) \
	    --builddir="$(BUILDDIR)")):$$PATH; \
	fi; \
	$(NHMTOOL) deps $(PKGNAME) -d "$(BUILDDIR)" >> $(GHCENV)
	(cd test; ln -sf ../$(GHCENV) $(GHCENVLNK))
	ln -sf $(GHCENV) $(GHCENVLNK)

$(TESTDIR)/$(GHCENV): $(GHCENVLNK)
	cp -u $(GHCENV) $(TESTDIR)

$(TESTDIR)/LabeledMediaRouter.hs:
	(cd $(TESTDIR); ln -s ../lmr.hs LabeledMediaRouter.hs)

$(SETUPCONFIG): $(GHCENVLNK)
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

$(BENCH): $(SETUPCONFIG) $(SRC) $(BENCH).hs
	ghc $(SRC) $(BENCH).hs -o $(BENCH)

install: $(DISTR)
	install -d $(PREFIX)
	tar xf $(DISTR) -C $(PREFIX) --no-same-owner

clean:
	rm -rf $(DEPLIBS)
	rm -f $(OBJS) $(HIOBJS) $(DYNOBJS) $(DYNHIOBJS) $(STUBS)
	rm -f $(LIB)

clean-all: clean
	rm -rf $(BUILDDIR)
	rm -f $(GHCENV) $(GHCENVLNK) test/$(GHCENVLNK) $(DISTR) $(BENCH)
	rm -f $(TESTDIR)/$(GHCENV) $(TESTDIR)/LabeledMediaRouter.hs

