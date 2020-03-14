{ compiler ? "ghc882"

, rev    ? "8da81465c19fca393a3b17004c743e4d82a98e4f"
, sha256 ? "1f3s27nrssfk413pszjhbs70wpap43bbjx2pf4zq5x2c1kd72l6y"
, pkgs   ?
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowUnfree = true;
      config.allowBroken = false;
    }

, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation   ? null
}:

pkgs.haskellPackages.developPackage {
  root = ./.;

  overrides = self: super: {
  };

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    benchmarkDepends = (attrs.buildToolDepends or []) ++ [
      pkgs.haskellPackages.criterion
    ];

    doBenchmark = true;
  });

  inherit returnShellEnv;
}
