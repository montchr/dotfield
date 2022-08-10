# TODO: add dhall utils
{
  pkgs,
  extraModulesPath,
  inputs,
  lib,
  ...
}: let
  # These packages are available only in the default `pkgs` via external
  # overlays. We also need `system` to get the appropriate package set from the
  # unstable channel.
  inherit
    (pkgs)
    agenix
    nvfetcher-bin
    system
    ;
  inherit (pkgs.stdenv.buildPlatform) isLinux isi686;

  packages = inputs.nixos-unstable.legacyPackages.${system};

  inherit
    (packages)
    alejandra
    cachix
    editorconfig-checker
    nixUnstable
    rage
    shellcheck
    shfmt
    ssh-to-age
    terraform
    treefmt
    ;

  inherit (packages.nodePackages) prettier;

  nixos-generators = inputs.nixos-generators.defaultPackage.${pkgs.system};

  hooks = import ./hooks;

  withCategory = category: attrset: attrset // {inherit category;};
  pkgWithCategory = category: package: {inherit package category;};

  dotfield = pkgWithCategory "dotfield";
  linter = pkgWithCategory "linters";
  formatter = pkgWithCategory "formatters";
  utils = withCategory "utils";
  secrets = pkgWithCategory "secrets";
in {
  _file = toString ./.;

  name = "Dotfield";

  commands =
    [
      (dotfield nixUnstable)
      (dotfield inputs.deploy.packages.${pkgs.system}.deploy-rs)
      (dotfield terraform)
      (dotfield treefmt)

      {
        category = "dotfield";
        name = nvfetcher-bin.pname;
        help = nvfetcher-bin.meta.description;
        command = "cd $PRJ_ROOT/packages/sources; ${nvfetcher-bin}/bin/nvfetcher -c ./sources.toml $@";
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

      (secrets agenix)
      (secrets rage)
      (secrets ssh-to-age)
      {
        category = "secrets";
        name = "convert-keys";
        help = "helper to convert the usual ssh ed25519 keys to age keys";
        command = ''
          ${ssh-to-age}/bin/ssh-to-age -private-key -i ~/.ssh/id_ed25519 > ~/.config/sops/age/age-key.sec
          ${ssh-to-age}/bin/ssh-to-age -i ~/.ssh/id_ed25519.pub > ~/.config/sops/age/age-key.pub
        '';
      }
    ]
    ++ lib.optional (!isi686) (dotfield cachix)
    ++ lib.optional isLinux (dotfield nixos-generators);
}
