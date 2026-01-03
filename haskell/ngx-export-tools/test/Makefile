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
DISTR := $(PKGNAME)-$(PKGVER).tar.gz

OBJS := $(SRC:.hs=.o)
HIOBJS := $(SRC:.hs=.hi)
DYNOBJS := $(SRC:.hs=.dyn_o)
DYNHIOBJS := $(SRC:.hs=.dyn_hi)
STUBS := $(SRC:.hs=_stub.h)

GHC := ghc
CABAL := cabal
GHCVER := $(shell $(GHC) --numeric-version)
CABALVER := $(shell $(CABAL) --numeric-version)
GHCPUID := $(shell \
    if printf '3.12\n$(CABALVER)' | sort -VC && \
       printf '9.8\n$(GHCVER)' | sort -VC; \
    then ghcpuid=`$(GHC) --info | sed -n \
             's/^.*\x28"Project Unit Id","\(.*\)"\x29.*$$/\1/ip'`; \
        if test -z "$$ghcpuid"; \
        then echo "ghc-$(GHCVER)"; \
        else echo "$$ghcpuid"; \
        fi; \
    else echo "ghc-$(GHCVER)"; \
    fi)
GHCENV := .ghc.environment.$(MACHINE)-$(KERNEL)-$(GHCVER)
DEPLIBS := $(MACHINE)-$(KERNEL)-$(GHCPUID)
BUILDDIR := dist-nhm
SETUPCONFIG := $(BUILDDIR)/setup-config

INPLACE := $(shell grep -q '^packages:.*\.\./ngx-export-tools' \
                       cabal.project && echo 1 || echo 0)

.PHONY: all env config install clean clean-all

all: $(DISTR)

env: $(GHCENV)

config: $(SETUPCONFIG)

$(GHCENV): cabal.project $(PKGNAME).cabal
	$(CABAL) install --builddir="$(BUILDDIR)" --lib --only-dependencies \
	  --package-env .
ifeq ($(INPLACE),1)
	$(CABAL) build --builddir="$(BUILDDIR)" ngx-export-tools
	echo "package-db $$(pwd)/$(BUILDDIR)/packagedb/ghc-$(GHCVER)" >> \
	  $(GHCENV)
endif
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
	  --ghc-options="$(SRC) -o $(LIB) -package-env $(GHCENV) "` \
	    `"-ddump-splices $(LINKRTS)"

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

