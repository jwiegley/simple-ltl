{ compiler ? "ghc864"

, doBenchmark ? false
, doTracing   ? false
, doOptimize  ? false # enables GHC optimizations for production use
, doProfiling ? false # enables profiling support in GHC
, doStrict    ? false

, withHoogle  ? true

, rev    ? "3cd3d1eeb6b26f3acb9e9e16cd7220cd5eb07148"
, sha256 ? "0671riiyzw2y3vw2apxhnq6vq67py64cqkgwiajfnw5qcrva86pw"

, pkgs   ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "hnix requires at least nix 2.0"
    else import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
           inherit sha256; }) {
           config.allowUnfree = true;
           config.allowBroken = false;
         }

, mkDerivation   ? null
}:

pkgs.haskellPackages.developPackage {
  name = "log-analyzer";
  root = ./.;

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    buildTools = (attrs.buildTools or []) ++ [
    ];

    enableLibraryProfiling = doProfiling;
    enableExecutableProfiling = doProfiling;

    testHaskellDepends = attrs.testHaskellDepends ++ [
    ];

    inherit doBenchmark;

    passthru = {
      nixpkgs = pkgs;
    };
  });

  returnShellEnv = false;
}
