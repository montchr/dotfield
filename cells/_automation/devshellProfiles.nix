{
  inputs,
  cell,
}: let
  l = inputs.nixpkgs.lib // builtins;
  pkgs = inputs.nixpkgs;

  inherit (pkgs.stdenv) isLinux;
  inherit (inputs.deadnix.packages) deadnix;
  inherit (inputs.deploy-rs.packages) deploy-rs;
  inherit (inputs.nixpkgs-fork-add-lint-staged.legacyPackages) lint-staged;

  inherit
    (pkgs)
    alejandra
    cachix
    editorconfig-checker
    just
    nix-diff
    nix-tree
    nodejs
    nvd
    nvfetcher
    shellcheck
    statix
    treefmt
    ;
  inherit
    (pkgs.nodePackages)
    prettier
    yarn
    ;

  pkgWithCategory = category: package: {inherit package category;};

  dotfield = pkgWithCategory "dotfield";
  linters = pkgWithCategory "linters";
  formatters = pkgWithCategory "formatters";
  utils = pkgWithCategory "utils";

  mozilla-addons-to-nix-wrapped = {
    name = "mozilla-addons-to-nix";
    category = "utils";
    help = "Generate a Nix package set of Firefox add-ons from a JSON manifest.";
    command = ''
      nix run sourcehut:~rycee/mozilla-addons-to-nix -- $@
    '';
  };

  commonCommands = [
    ##: --- utils --------------------

    (utils cachix)
    (utils just)
    (utils nix-diff)
    (utils nix-tree)
    (utils nvd)
    mozilla-addons-to-nix-wrapped

    ##: --- linters ------------------

    (linters deadnix)
    (linters statix)

    ##: --- formatters ---------------

    (formatters alejandra)
    (formatters prettier)
    (formatters treefmt)
  ];

  linuxCommands = [
    (dotfield deploy-rs)
    (dotfield inputs.nixos-generators.packages.nixos-generate)
  ];
in {
  default = _: {
    commands =
      commonCommands
      ++ (l.optionals isLinux linuxCommands);

    packages = [
      cachix
      editorconfig-checker
      lint-staged
      shellcheck
      nodejs
      nvfetcher
      yarn
    ];
  };
}
