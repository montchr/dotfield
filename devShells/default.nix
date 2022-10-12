{
  self,
  lib,
  ...
}: {
  perSystem = {
    inputs',
    system,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) getExe optionals;
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
      cachix
      editorconfig-checker
      nix-diff
      nvfetcher
      rage
      fup-repl
      shellcheck
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
    linters = pkgWithCategory "linters";
    formatters = pkgWithCategory "formatters";
    utils = pkgWithCategory "utils";
    utils' = withCategory "utils";
    secrets = pkgWithCategory "secrets";
    secrets' = withCategory "secrets";

    updateSources = dotfield' (wrapPkg nvfetcher ''
      pushd $PRJ_ROOT/packages/sources
      ${getExe nvfetcher} -c ./sources.toml $@
      popd
    '');

    updateFirefoxAddons = dotfield' {
      name = "do-update-firefox-addons";
      help = "Generate a Nix package set of Firefox add-ons from a JSON manifest.";
      # N.B. As a Haskell package, including this flake's default package
      # directly will break our own `nix flake check` due to IFD.
      #
      # The current method here relies on the flake being available in the
      # registry, which happens automatically when added as an input. We
      # could, instead, just as easily reference the flake's upstream URL in
      # the command, but would then lose the benefits of pinning inputs.
      command = ''
        cd $PRJ_ROOT/packages/applications/firefox/firefox-addons
        nix run mozilla-addons-to-nix -- addons.json addons.nix
        cd $PRJ_ROOT
      '';
    };

    commonCommands = [
      (dotfield' {
        name = "do-format";
        command = ''
          treefmt --clear-cache -- "$@"
        '';
      })
      (dotfield terraform)
      (dotfield' {
        name = "do-repl";
        help = "a REPL for the system flake, by flake-utils-plus";
        command = "${getExe fup-repl} $@";
      })
      (dotfield' {
        name = "do-update-all";
        help = "update all of the things and switch to a new generation";
        command = ''
          export CACHIX_AUTH_TOKEN="$(${getExe pkgs.pass} show cachix.org/auth-token)"
          nix flake update --verbose
          ${updateSources.command}
          ${updateFirefoxAddons.command}
          doom profiles sync
          doom upgrade
          ${getExe cachix} watch-exec --jobs 2 dotfield \
            ${rebuildSystem} -- switch --verbose
        '';
      })

      updateSources
      updateFirefoxAddons

      (utils nix-diff)
      (utils' {
        name = "evalnix";
        help = "Check Nix parsing";
        command = ''
          ${getExe pkgs.fd} --extension nix --exec \
            nix-instantiate --parse --quiet {} >/dev/null
        '';
      })

      (linters editorconfig-checker)
      (linters shellcheck)
      (formatters prettier)
      (formatters treefmt)

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
          ${getExe ssh-to-age} -private-key -i ~/.ssh/id_ed25519 > $SOPS_AGE_KEY_DIR/age-key.sec
          ${getExe ssh-to-age} -i ~/.ssh/id_ed25519.pub > $SOPS_AGE_KEY_DIR/age-key.pub
        '';
      })
    ];

    linuxCommands = [
      (dotfield deploy-rs)
      (dotfield inputs'.nixos-generators.packages.nixos-generators)
    ];
  in {
    devShells.default = inputs'.devshell.legacyPackages.mkShell ({extraModulesPath, ...}: {
      imports = ["${extraModulesPath}/git/hooks.nix"];

      name = "dotfield";

      # FIXME: very not ready for usage, don't even try
      # git.hooks.enable = true;
      # git.hooks.pre-commit.text = builtins.readFile ./git/hooks/pre-commit.sh;

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
        ++ (optionals isLinux linuxCommands);

      packages = [
        cachix
        sops-import-keys-hook
        sops-init-gpg-key
        ssh-to-pgp
      ];
    });
  };
}
