{
  inputs',
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.stdenv) isLinux;
  l = inputs.nixpkgs.lib // builtins;
in {
  packages =
    [
      pkgs.alejandra
      pkgs.cachix
      pkgs.deadnix
      pkgs.editorconfig-checker
      pkgs.gh
      pkgs.hello
      pkgs.just
      pkgs.nix-diff
      pkgs.nix-tree
      pkgs.nodePackages.prettier
      pkgs.nodePackages.yarn
      pkgs.nodejs
      pkgs.nvd
      pkgs.nvfetcher
      pkgs.reuse
      pkgs.shellcheck
      pkgs.statix
      pkgs.treefmt
    ]
    ++ (l.optional isLinux [
      inputs'.deploy-rs.packages.deploy-rs
      inputs'.nixos-generators.packages.nixos-generate
    ]);
  enterShell = ''
    hello
  '';
  scripts = {
    mozilla-addons-to-nix.exec = ''
      nix run sourcehut:~rycee/mozilla-addons-to-nix -- $@
    '';
  };
  # FIXME: errors out
  # starship.enable = true;
}
