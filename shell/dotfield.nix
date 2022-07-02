{
  pkgs,
  extraModulesPath,
  inputs,
  lib,
  ...
}: let
  inherit
    (pkgs)
    agenix
    alejandra
    cachix
    editorconfig-checker
    nixUnstable
    nvfetcher-bin
    shellcheck
    shfmt
    terraform
    treefmt
    ;

  inherit (pkgs.nodePackages) prettier;

  hooks = import ./hooks;

  withCategory = category: attrset: attrset // {inherit category;};
  pkgWithCategory = category: package: {inherit package category;};

  dotfield = pkgWithCategory "dotfield";
  linter = pkgWithCategory "linters";
  formatter = pkgWithCategory "formatters";
  utils = withCategory "utils";
in {
  _file = toString ./.;

  commands =
    [
      (dotfield nixUnstable)
      (dotfield agenix)
      (dotfield inputs.deploy.packages.${pkgs.system}.deploy-rs)
      (dotfield terraform)
      (dotfield treefmt)

      {
        category = "dotfield";
        name = nvfetcher-bin.pname;
        help = nvfetcher-bin.meta.description;
        command = "cd $PRJ_ROOT/pkgs; ${nvfetcher-bin}/bin/nvfetcher -c ./sources.toml $@";
      }

      (utils {
        name = "evalnix";
        help = "Check Nix parsing";
        command = "fd --extension nix --exec nix-instantiate --parse --quiet {} >/dev/null";
      })

      (formatter alejandra)
      (formatter prettier)
      (formatter shfmt)

      (linter editorconfig-checker)
      (linter shellcheck)
    ]
    ++ lib.optional (!pkgs.stdenv.buildPlatform.isi686)
    (dotfield cachix)
    ++ lib.optional (pkgs.stdenv.hostPlatform.isLinux && !pkgs.stdenv.buildPlatform.isDarwin)
    (dotfield inputs.nixos-generators.defaultPackage.${pkgs.system});
}
