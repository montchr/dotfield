# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  self,
  lib,
  ...
}:
let
  sources = import (self.outPath + "/npins");
in
{
  perSystem =
    {
      config,
      inputs',
      pkgs,
      ...
    }:
    let
      nix-unit = inputs'.nix-unit.packages.default;

      commonPkgs = with pkgs; [
        bashInteractive
        biome
        jq
        just
        npins
        pnpm
      ];

      checksPkgs = [
        nix-unit

        pkgs.deadnix
        pkgs.editorconfig-checker
        pkgs.shellcheck
        pkgs.statix
      ];

      formatterPkgs = [
        pkgs.treefmt
        pkgs.actionlint
        pkgs.biome
        pkgs.kdlfmt
        pkgs.keep-sorted
        pkgs.mdformat
        pkgs.nixfmt
        pkgs.shfmt
        pkgs.taplo
        pkgs.yamlfmt
      ];

      deploymentPkgs = [
        inputs'.colmena.packages.colmena
      ];

      nixifyPkgs = [
        pkgs.crudini # for dconf dump editing
        pkgs.yq-go
      ];

      secretsPkgs = [
        inputs'.sops-nix.packages.sops-import-keys-hook
        inputs'.sops-nix.packages.ssh-to-pgp
        inputs'.sops-nix.packages.sops-init-gpg-key

        pkgs.age-plugin-yubikey
        pkgs.rage
        pkgs.sops
        pkgs.ssh-to-age
      ];

      maintenancePkgs = [
        # FIXME: manpage build failure on nixos-unstable
        # pkgs.reuse
        inputs'.nixos-stable.legacyPackages.reuse
      ];

      developmentPkgs =
        commonPkgs
        ++ checksPkgs
        ++ formatterPkgs
        ++ maintenancePkgs
        ++ nixifyPkgs
        ++ secretsPkgs
        ++ config.pre-commit.settings.enabledPackages
        ++ [
          pkgs.cachix
          pkgs.nix-init
          pkgs.nix-inspect
          pkgs.nixdoc
          pkgs.stow
        ];

      ciPkgs = commonPkgs ++ [ nix-unit ];

      envVars = { };

      shellHook = ''
        source ${sources.prj-spec}/contrib/shell-hook.sh

        ${lib.strings.toShellVars envVars}

        ${config.pre-commit.installationScript}
      '';

    in
    {
      devShells.default = config.devShells.dotfield;

      devShells.ci = pkgs.mkShell {
        inherit shellHook;
        name = "dotfield-ci";
        nativeBuildInputs = ciPkgs;
      };

      devShells.dotfield = pkgs.mkShell {
        inherit shellHook;
        name = "dotfield";
        nativeBuildInputs = developmentPkgs ++ [ ];
      };

      devShells.secrets = pkgs.mkShell {
        name = "dotfield-secrets";
        nativeBuildInputs = secretsPkgs;
      };
    };
}
