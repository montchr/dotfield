{self, ...}: {
  perSystem = {
    inputs',
    system,
    pkgs,
    lib,
    ...
  }: let
    inherit (pkgs.stdenv) isDarwin isLinux;
    inherit (inputs'.agenix.packages) agenix;
    inherit (inputs'.deadnix.packages) deadnix;
    inherit (inputs'.deploy-rs.packages) deploy-rs;
    inherit
      (inputs'.sops-nix.packages)
      sops-import-keys-hook
      sops-init-gpg-key
      ssh-to-pgp
      ;
    inherit (inputs'.nixpkgs-fork-add-lint-staged.legacyPackages) lint-staged;

    inherit
      (pkgs)
      alejandra
      cachix
      editorconfig-checker
      just
      manix
      nix-diff
      nix-tree
      nodejs
      nvd
      nvfetcher
      rage
      shellcheck
      ssh-to-age
      statix
      sops
      treefmt
      ;
    inherit
      (pkgs.nodePackages)
      prettier
      yarn
      ;

    l = lib // builtins;

    rebuildSystem =
      if isDarwin
      then "darwin-rebuild"
      else "nixos-rebuild";

    withCategory = category: attrset: attrset // {inherit category;};
    pkgWithCategory = category: package: {inherit package category;};

    dotfield = pkgWithCategory "dotfield";
    dotfield' = withCategory "dotfield";
    ui' = withCategory "ui";
    linters = pkgWithCategory "linters";
    linters' = withCategory "linters";
    formatters = pkgWithCategory "formatters";
    formatters' = withCategory "formatters";
    utils = pkgWithCategory "utils";
    utils' = withCategory "utils";
    secrets = pkgWithCategory "secrets";
    secrets' = withCategory "secrets";

    cacheName = "dotfield";
    cachixExec = ''${l.getExe cachix} watch-exec --jobs 2 ${cacheName}'';

    mozilla-addons-to-nix-wrapped = utils' {
      name = "mozilla-addons-to-nix";
      help = "Generate a Nix package set of Firefox add-ons from a JSON manifest.";
      command = ''
        nix run sourcehut:~rycee/mozilla-addons-to-nix -- $@
      '';
    };

    commonCommands = [
      ##: --- utils --------------------

      (utils cachix)
      (utils just)
      (utils manix)
      (utils nix-diff)
      (utils nix-tree)
      mozilla-addons-to-nix-wrapped

      ##: --- linters ------------------

      (linters editorconfig-checker)
      (linters shellcheck)
      (linters statix)

      ##: --- formatters ---------------

      (formatters alejandra)
      (formatters prettier)
      (formatters treefmt)

      ##: --- secrets ------------------

      (secrets agenix)
      (secrets rage)
      (secrets sops)
      (secrets ssh-to-age)
      (secrets' {
        name = "install-age-key";
        help = "copy the age secret key from the password-store into place";
        command = ''
          mkdir -p $SOPS_AGE_KEY_DIR
          ${pkgs.pass}/bin/pass show age--secret-key >> $SOPS_AGE_KEY_FILE
        '';
      })
      (secrets' {
        name = "convert-ssh-to-age-key";
        help = "helper to convert the usual ssh ed25519 keys to age keys";
        command = ''
          mkdir -p $SOPS_AGE_KEY_DIR
          ${l.getExe ssh-to-age} -private-key -i ~/.ssh/id_ed25519 > $SOPS_AGE_KEY_DIR/age-key.sec
          ${l.getExe ssh-to-age} -i ~/.ssh/id_ed25519.pub > $SOPS_AGE_KEY_DIR/age-key.pub
        '';
      })
    ];

    linuxCommands = [
      (dotfield deploy-rs)
      (dotfield inputs'.nixos-generators.packages.nixos-generators)
    ];
  in {
    devShells.default = inputs'.devshell.legacyPackages.mkShell ({...}: {
      name = "dotfield";

      # TODO
      # sopsPGPKeyDirs = ["./nixos/secrets/keys"];
      # sopsCreateGPGHome = true;

      env = [
        {
          name = "SOPS_AGE_KEY_DIR";
          eval = "$XDG_CONFIG_HOME/sops/age";
        }
        {
          name = "SOPS_AGE_KEY_FILE";
          eval = "$XDG_CONFIG_HOME/sops/age/keys";
        }
        {
          name = "AGENIX_ROOT";
          eval = "$PRJ_ROOT";
        }
      ];

      commands =
        commonCommands
        ++ (l.optionals isLinux linuxCommands);

      packages = [
        cachix
        deadnix
        lint-staged
        sops-import-keys-hook
        sops-init-gpg-key
        ssh-to-pgp
        nodejs
        yarn
      ];
    });
  };
}
