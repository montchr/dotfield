{ inputs, lib, ... }:
{
  perSystem =
    {
      config,
      inputs',
      pkgs,
      ...
    }:
    let
      commonPkgs = with pkgs; [
        biome
        just
      ];

      checksPkgs = with pkgs; [
        biome
        deadnix
        editorconfig-checker
        shellcheck
        statix
      ];

      formatterPkgs = with pkgs; [
        nixfmt-rfc-style
        nodePackages.prettier
        treefmt
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
        ++ [
          inputs'.nix-inspect.packages.default
          pkgs.cachix
          pkgs.nix-init
          pkgs.nixdoc
        ];

      ciPkgs = commonPkgs;

      envVars = { };

      shellHook = ''
        source ${inputs.prj-spec}/contrib/shell-hook.sh

        ${lib.strings.toShellVars envVars}
      '';

    in
    {
      devShells.default = config.devShells.dotfield;

      devShells.ci = pkgs.mkShell {
        inherit shellHook;
        nativeBuildInputs = ciPkgs;
      };

      devShells.dotfield = pkgs.mkShell {
        inherit shellHook;
        nativeBuildInputs = developmentPkgs ++ [ ];
      };

      devShells.secrets = pkgs.mkShell {
        nativeBuildInputs = secretsPkgs;
      };
    };
}
