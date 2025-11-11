{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  devShells.default = import ./shell.nix { inherit inputs pkgs lib project; };

  projectFlake = project.flake {};

  packages = projectFlake.packages or {};

  checks = projectFlake.checks or {};

in

{
  inherit packages;
  inherit devShells;
  inherit checks;
}
