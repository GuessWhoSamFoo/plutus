########################################################################
# default.nix -- The top-level nix build file for Plutus.
#
# This file defines various attributes that are used for building and
# developing Plutus.
#
########################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { allowUnfreePredicate = (import ./nix/lib/unfree.nix).unfreePredicate; }
  # Overrides for niv
, sourcesOverride ? { }
, packages ? import ./nix { inherit system crossSystem config sourcesOverride rev checkMaterialization enableHaskellProfiling; }
  # An explicit git rev to use, passed when we are in Hydra
, rev ? null
  # Whether to check that the pinned shas for haskell.nix are correct. We want this to be
  # false, generally, since it does more work, but we set it to true in the CI
, checkMaterialization ? false
  # Whether to build our Haskell packages (and their dependencies) with profiling enabled.
, enableHaskellProfiling ? false
}:
let
  inherit (packages) pkgs plutus sources;
  inherit (pkgs) lib haskell-nix;
  inherit (plutus) haskell iohkNix git-rev set-git-rev agdaPackages;
  inherit (plutus) easyPS sphinxcontrib-haddock;
in
rec {
  inherit pkgs plutus;

  inherit (plutus) web-ghc;

  inherit (haskell.packages.plutus-pab.components.exes)
    plutus-game
    plutus-currency
    plutus-atomic-swap
    plutus-pay-to-wallet;

  inherit (haskell.packages.marlowe.components.exes) marlowe-app;

  webCommon = pkgs.callPackage ./web-common { inherit (plutus.lib) gitignore-nix; };
  webCommonPlutus = pkgs.callPackage ./web-common-plutus { inherit (plutus.lib) gitignore-nix; };
  webCommonMarlowe = pkgs.callPackage ./web-common-marlowe { inherit (plutus.lib) gitignore-nix; };
  webCommonPlayground = pkgs.callPackage ./web-common-playground { inherit (plutus.lib) gitignore-nix; };

  plutus-playground = pkgs.recurseIntoAttrs rec {
    tutorial = docs.site;
    haddock = plutus.plutus-haddock-combined;

    inherit (pkgs.callPackage ./plutus-playground-client {
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit set-git-rev haskell webCommon webCommonPlutus webCommonPlayground;
    }) client server-invoker generated-purescript generate-purescript start-backend;
  };

  marlowe-playground = pkgs.recurseIntoAttrs rec {
    tutorial = docs.marlowe-tutorial;

    inherit (pkgs.callPackage ./marlowe-playground-client {
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit set-git-rev haskell webCommon webCommonMarlowe webCommonPlayground;
    }) client server-invoker generated-purescript generate-purescript start-backend;
  };

  marlowe-dashboard = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./marlowe-dashboard-client {
      inherit plutus-pab;
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit webCommon webCommonMarlowe;
    }) client server-invoker generated-purescript generate-purescript;
  };

  marlowe-marketplace = pkgs.recurseIntoAttrs rec {
    inherit (pkgs.callPackage ./marlowe-marketplace-client {
      inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      inherit webCommon webCommonMarlowe;
    }) client;
  };

  plutus-pab = pkgs.recurseIntoAttrs (pkgs.callPackage ./plutus-pab-client {
    inherit (plutus.lib) buildPursPackage buildNodeModules gitignore-nix filterNpm;
    inherit set-git-rev haskell webCommon webCommonPlutus;
  });

  tests = import ./nix/tests/default.nix {
    inherit pkgs iohkNix;
    inherit (plutus) fixStylishHaskell fixPurty;
    inherit (pkgs) terraform;
    src = ./.;
  };

  docs = import ./nix/docs.nix { inherit pkgs plutus; };

  deployment = pkgs.callPackage ./deployment {
    inherit plutus marlowe-playground plutus-playground;
  };

  # Build the shell expression to be sure it works on all platforms
  #
  # The shell should never depend on any of our Haskell packages, which can
  # sometimes happen by accident. In practice, everything depends transitively
  # on 'plutus-core', so this does the job.
  # FIXME: this should simply be set on the main shell derivation, but this breaks
  # lorri: https://github.com/target/lorri/issues/489. In the mean time, we set it
  # only on the CI version, so that we still catch it, but lorri doesn't see it.
  shell = (import ./shell.nix { inherit packages; }).overrideAttrs (attrs: attrs // {
    disallowedRequisites = [ plutus.haskell.packages.plutus-core.components.library ];
  });

  # WIP to make a VS Code devcontainer that can be used for working on plutus code
  #   docker load < $(nix-build --system x86_64-linux -A devcontainer)
  # In VS Code install:
  #   https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers
  devcontainer =
    pkgs.callPackage (import ./devcontainer) {
      name = "plutus-devcontainer";
      tag = "latest";
      nixpkgsPath = pkgs.path;
      extraContents = [
        shell.ghc
        plutus.haskell-language-server
        plutus.cabal-install
        pkgs.zsh
      ];
    };
}
