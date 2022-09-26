{lib, ...}: let
  inherit (lib) getBin;
in {
  perSystem = {
    inputs',
    system,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) optionals;
    inherit (pkgs.stdenv) isDarwin isLinux;
    inherit (inputs'.agenix.packages) agenix;
    inherit (inputs'.deploy-rs.packages) deploy-rs;
    inherit
      (inputs'.sops-nix.packages)
      sops-import-keys-hook
      sops-init-gpg-key
      ssh-to-pgp
      ;

    inherit
      (pkgs)
      alejandra
      cachix
      editorconfig-checker
      nix-diff
      nvfetcher
      rage
      fup-repl
      shellcheck
      shfmt
      ssh-to-age
      sops
      terraform
      treefmt
      ;
    inherit (pkgs.nodePackages) prettier;

    rebuildSystem =
      if isDarwin
      then "darwin-rebuild"
      else "nixos-rebuild";

    withCategory = category: attrset: attrset // {inherit category;};
    pkgWithCategory = category: package: {inherit package category;};
    wrapPkg = package: command: {
      inherit command;
      name = package.pname;
      help = package.meta.description;
    };

    dotfield = pkgWithCategory "dotfield";
    dotfield' = withCategory "dotfield";
    linter = pkgWithCategory "linters";
    formatter = pkgWithCategory "formatters";
    utils = pkgWithCategory "utils";
    utils' = withCategory "utils";
    secrets = pkgWithCategory "secrets";
    secrets' = withCategory "secrets";

    updateSources = dotfield' (wrapPkg nvfetcher ''
      pushd $PRJ_ROOT/packages/sources
      ${nvfetcher}/bin/nvfetcher -c ./sources.toml $@
      popd
    '');

    updateFirefoxAddons = utils' {
      name = "mozilla-addons-to-nix";
      help = "Generate a Nix package set of Firefox add-ons from a JSON manifest.";
      # N.B. As a Haskell package, including this flake's default package
      # directly will break our own `nix flake check` due to IFD.
      #
      # The current method here relies on the flake being available in the
      # registry, which happens automatically when added as an input. We
      # could, instead, just as easily reference the flake's upstream URL in
      # the command, but would then lose the benefits of pinning inputs.
      command = ''
        pushd $PRJ_ROOT/packages/applications/firefox/firefox-addons
        nix run mozilla-addons-to-nix -- addons.json addons.nix
        popd
      '';
    };

    commonCommands = [
      (dotfield deploy-rs)
      (dotfield terraform)
      (dotfield' {
        name = "do-repl";
        help = "a REPL for the system flake, by flake-utils-plus";
        command = "${getBin fup-repl} $@";
      })

      updateSources
      updateFirefoxAddons

      (dotfield' {
        name = "do-update-all";
        help = "update all of the things and switch to a new generation";
        command = ''
          nix flake update --verbose
          ${updateSources.command}
          ${updateFirefoxAddons.command}
          doom profiles sync
          doom upgrade
          ${getBin cachix} watch-exec --jobs 2 dotfield \
            ${rebuildSystem} -- switch --verbose
        '';
      })

      (utils nix-diff)
      (utils' {
        name = "evalnix";
        help = "Check Nix parsing";
        command = ''
          ${getBin pkgs.fd} --extension nix --exec \
            nix-instantiate --parse --quiet {} >/dev/null
        '';
      })

      (formatter alejandra)
      (formatter prettier)
      (formatter shfmt)
      (formatter treefmt)

      (linter editorconfig-checker)
      (linter shellcheck)

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
          ${getBin ssh-to-age} -private-key -i ~/.ssh/id_ed25519 > $SOPS_AGE_KEY_DIR/age-key.sec
          ${getBin ssh-to-age} -i ~/.ssh/id_ed25519.pub > $SOPS_AGE_KEY_DIR/age-key.pub
        '';
      })
    ];

    linuxCommands = [
      (dotfield inputs'.nixos-generators.packages.nixos-generators)
    ];
  in {
    devShells.default = inputs'.devshell.legacyPackages.mkShell {
      name = "dotfield";

      # git.hooks.enable = true;
      # git.hooks.pre-commit.text = ''${pkgs.treefmt}/bin/treefmt'';
      # sopsPGPKeyDirs = ["./nixos/secrets/keys"];
      # sopsCreateGPGHome = true;
      # nativeBuildInputs = [
      #   # inputs'.deploykit.packages.deploykit
      #   # pkgs.python3.pkgs.invoke
      # ];

      env = [
        {
          name = "SOPS_AGE_KEY_DIR";
          eval = "$XDG_CONFIG_HOME/sops/age";
        }
        {
          name = "SOPS_AGE_KEY_FILE";
          eval = "$XDG_CONFIG_HOME/sops/age/keys";
        }
      ];

      commands =
        commonCommands
        ++ (optionals isLinux linuxCommands);

      packages = [
        cachix
        sops-import-keys-hook
        sops-init-gpg-key
        ssh-to-pgp
      ];
    };
  };
}
