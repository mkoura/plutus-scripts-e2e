{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  devShells.default = import ./shell.nix {
    inherit
      inputs
      pkgs
      lib
      project
      ;
  };

  projectFlake = project.flake { };

  packages = projectFlake.packages or { };

  checks = projectFlake.checks or { };

  # Configure nix fmt to use treefmt
  formatter = pkgs.writeShellApplication {
    name = "treefmt";
    runtimeInputs = with pkgs; [
      treefmt
      fourmolu
      haskellPackages.cabal-fmt
      nixfmt-rfc-style
      nodePackages.prettier
    ];
    text = ''
      exec treefmt "$@"
    '';
  };

in

{
  inherit packages;
  inherit devShells;
  inherit checks;
  inherit formatter;
}
