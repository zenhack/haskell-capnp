NPROCS ?= 1
OS := $(shell uname -s)

ifeq ($(OS),Linux)
  NPROCS := $(shell nproc)
endif
ifeq ($(OS),Darwin)
  NPROCS := $(shell sysctl -n hw.ncpu)
endif
ifeq ($(OS),FreeBSD)
  NPROCS := $(shell sysctl -n hw.ncpu)
endif

# This should ideally be updated from the nixos-17.09 branch of the repository
# at https://github.com/NixOS/nixpkgs-channels
NIXPKGS_REVISION = 788ce6e3df12bb0cf19fb9ccf8ffa75558b551ba

.PHONY: all help configure build       \
        lib-repl exe-repl test-repl    \
        nix-build nix-shell update-nix

all: help

help:
	-@echo "Targets:"
	-@echo "  configure:  cabal configure"
	-@echo "  build:      cabal build"
	-@echo "  lib-repl:   cabal repl lib:capnp"
	-@echo "  exe-repl:   cabal repl exe:capnpc-haskell"
	-@echo "  test-repl:  cabal repl test:the-test-suite"
	-@echo "  nix-build:  build via nix"
	-@echo "  nix-shell:  open a nix shell"
	-@echo "  update-nix: update any pinned nix files"

configure:
	cabal configure --enable-tests

build: configure
	cabal build -j

lib-repl: configure
	cabal repl "lib:capnp"

exe-repl: configure
	cabal repl "exe:capnpc-haskell"

test-repl: configure
	cabal repl "test:the-test-suite"

nix-build: nix/capnp.nix
	-@rm -f result*
	nix-build --no-out-link release.nix -A capnp -Q -j $(NPROCS)

# I've seen problems caused by not running cabal clean before entering a
# nix shell. In particular, I think the result of cabal configure can be
# inappropriately cached from executions in a different environment.
nix-shell: nix/capnp.nix
	cabal clean
	nix-shell --run $$SHELL

# another example of something that would go under this rule:
# cd nix; cabal2nix "https://github.com/zenhack/haskell-quota" > quota.nix
update-nix: nix/capnp.nix
	nix-prefetch-git "https://github.com/NixOS/nixpkgs" $(NIXPKGS_REVISION) \
	    > nix/nixpkgs.json

nix/capnp.nix: capnp.cabal
	cd nix; cabal2nix ../. > capnp.nix
