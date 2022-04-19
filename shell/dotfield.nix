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
    cachix
    editorconfig-checker
    nixUnstable
    nixpkgs-fmt
    # nvfetcher-bin
    
    ;

  hooks = import ./hooks;

  withCategory = category: attrset: attrset // {inherit category;};
  pkgWithCategory = category: package: {inherit package category;};

  dotfield = pkgWithCategory "dotfield";
  linter = pkgWithCategory "linter";
  utils = withCategory "utils";
in {
  _file = toString ./.;

  commands =
    [
      (dotfield nixUnstable)
      (dotfield agenix)
      # (dotfield inputs.deploy.packages.${pkgs.system}.deploy-rs)

      # {
      #   category = "dotfield";
      #   name = nvfetcher-bin.pname;
      #   help = nvfetcher-bin.meta.description;
      #   command = "cd $PRJ_ROOT/pkgs; ${nvfetcher-bin}/bin/nvfetcher -c ./sources.toml $@";
      # }

      (utils {
        name = "evalnix";
        help = "Check Nix parsing";
        command = "fd --extension nix --exec nix-instantiate --parse --quiet {} >/dev/null";
      })

      (linter nixpkgs-fmt)
      (linter editorconfig-checker)
    ]
    ++ lib.optional (!pkgs.stdenv.buildPlatform.isi686)
    (dotfield cachix)
    ++ lib.optional (pkgs.stdenv.hostPlatform.isLinux && !pkgs.stdenv.buildPlatform.isDarwin)
    (dotfield inputs.nixos-generators.defaultPackage.${pkgs.system});
}
