{
  self,
  inputs,
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
      difftastic-16k = pkgs.difftastic.overrideAttrs (oldAttrs: {
        JEMALLOC_SYS_WITH_LG_PAGE = "16";
      });

      nix-unit = inputs'.nix-unit.packages.default.overrideAttrs (oldAttrs: {
        postInstall = ''
          wrapProgram "$out/bin/nix-unit" --prefix PATH : ${difftastic-16k}/bin
        '';
      });

      commonPkgs = with pkgs; [
        biome
        just
        npins
      ];

      checksPkgs = [
        pkgs.biome
        pkgs.deadnix
        pkgs.editorconfig-checker
        nix-unit
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
        pkgs.nixfmt-rfc-style
        pkgs.shfmt
        pkgs.taplo
        pkgs.yamlfmt
      ];

      deploymentPkgs = [
        inputs'.colmena.packages.colmena
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

      maintenancePkgs = [ pkgs.reuse ];

      developmentPkgs =
        commonPkgs
        ++ checksPkgs
        ++ formatterPkgs
        ++ maintenancePkgs
        ++ config.pre-commit.settings.enabledPackages
        ++ [
          inputs'.nix-inspect.packages.default
          pkgs.cachix
          pkgs.nix-init
          pkgs.nixdoc
          pkgs.stow
        ];

      ciPkgs = commonPkgs;

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
