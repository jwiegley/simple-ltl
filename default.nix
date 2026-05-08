{ ghcCompiler ? "ghc8107"

, rev    ? "a3a23d9599b0a82e333ad91db2cdc479313ce154"
, sha256 ? "05xmgrrnw6j39lh3d48kg064z510i0w5vvrm1s5cdwhdc2fkspjq"
, pkgs   ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${ghcCompiler};

in haskellPackages.developPackage rec {
  # name = "haskell-${ghcCompiler}-simple-ltl";
  root = ./.;

  source-overrides = {};
  overrides = self: super: with pkgs.haskell.lib; {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
      haskellPackages.cabal-install
      haskellPackages.hpack
      haskellPackages.hoogle
      haskellPackages.hasktags
    ];

    benchmarkDepends = (attrs.benchmarkDepends or []) ++ [
      pkgs.haskellPackages.criterion
    ];

    doBenchmark = true;

    passthru = {
      nixpkgs = pkgs;
      inherit haskellPackages;
    };
  });

  inherit returnShellEnv;
}
