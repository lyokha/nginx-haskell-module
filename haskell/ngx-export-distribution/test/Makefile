NAME := ngx_distribution_test
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
GHCPUID := $(shell if test `echo $(CABALVER) 3.12 | sed 's/ /\n/' | \
                         sort -V | head -1` = 3.12 && \
                      test `echo $(GHCVER) 9.10 | sed 's/ /\n/' | \
                         sort -V | head -1` = 9.10; \
                   then ghcpuid=`$(GHC) --info | \
                            grep -i '"Project Unit Id"' | head -1 | \
                            sed 's/.\+,"\(.\+\)"\x29$$/\1/'`; \
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

INPLACE := $(shell grep -q '^packages:.*\.\./$(PKGDISTR)' \
                       cabal.project && echo 1 || echo 0)

.PHONY: all env config install clean clean-all

all: $(DISTR)

env: $(GHCENVLNK)

config: $(SETUPCONFIG)

$(GHCENVLNK): cabal.project $(PKGNAME).cabal
	rm -f $(GHCENVLNK)
	$(CABAL) install --builddir="$(BUILDDIR)" --lib --only-dependencies \
	  --package-env .
ifeq ($(INPLACE),1)
	$(CABAL) build --builddir="$(BUILDDIR)" $(PKGDISTR)
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
	ln -sf $(GHCENV) $(GHCENVLNK)

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

install: $(DISTR)
	install -d $(PREFIX)
	tar xf $(DISTR) -C $(PREFIX) --no-same-owner

clean:
	rm -rf $(DEPLIBS)
	rm -f $(OBJS) $(HIOBJS) $(DYNOBJS) $(DYNHIOBJS) $(STUBS)
	rm -f $(LIB)

clean-all: clean
	rm -rf $(BUILDDIR)
	rm -f $(GHCENV) $(GHCENVLNK) $(DISTR)

