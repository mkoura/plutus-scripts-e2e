{
  inputs,
  pkgs,
  lib,
}:

let
  cabalProject = pkgs.haskell-nix.cabalProject' (
    { config, pkgs, ... }:
    {
      name = "plutus-scripts-e2e";

      compiler-nix-name = lib.mkDefault "ghc96";

      src = lib.cleanSource ../.;

      inputMap = {
        "https://chap.intersectmbo.org/" = inputs.CHaP;
      };

      shell.withHoogle = false;

      modules = [
        {
          packages = { };
        }
      ];
    }
  );

in

cabalProject
