{ inputs, pkgs, lib, project }:

let
  # Get Haskell development tools from the project
  cabal = project.tool "cabal" "latest";
  hls = project.tool "haskell-language-server" "latest";

  shell = project.shellFor {
    name = "plutus-scripts-e2e-${project.args.compiler-nix-name}";

    buildInputs = [
      cabal
      hls
      pkgs.git
      pkgs.bash
      pkgs.cacert
    ];

    withHoogle = false;

    shellHook = ''
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
    '';
  };

in

shell
