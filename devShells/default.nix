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
    inherit (lib) getExe optionals optionalString;
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

    dotfield = pkgWithCategory "dotfield";
    dotfield' = withCategory "dotfield";
    linters = pkgWithCategory "linters";
    formatters = pkgWithCategory "formatters";
    utils = pkgWithCategory "utils";
    utils' = withCategory "utils";
    secrets = pkgWithCategory "secrets";
    secrets' = withCategory "secrets";

    # Mitigate issues with home-manager and kitty terminal on Darwin
    withTermFix = optionalString isDarwin ''TERM="xterm-256color"'';

    cacheName = "dotfield";
    withCachixToken = ''CACHIX_AUTH_TOKEN="$(${getExe pkgs.pass} show cachix.org/${cacheName}/auth-token)"'';
    cachixExec = ''${getExe cachix} watch-exec --jobs 2 ${cacheName}'';

    do-update-nvfetcher-sources = dotfield' {
      name = "do-update-nvfetcher-sources";
      help = nvfetcher.meta.description;
      command = ''
        pushd $PRJ_ROOT/packages/sources
        ${getExe nvfetcher} -c ./sources.toml $@
        popd
      '';
    };

    do-update-firefox-addons = dotfield' {
      name = "do-update-firefox-addons";
      help = "Generate a Nix package set of Firefox add-ons from a JSON manifest.";
      # N.B. As a Haskell package, including this flake's default package
      # directly will break our own `nix flake check` due to IFD. It will also break
      # CI SaaS (e.g. Garnix) when included as a flake input.
      command = ''
        cd $PRJ_ROOT/packages/applications/firefox/firefox-addons
        nix run sourcehut:~rycee/mozilla-addons-to-nix -- addons.json addons.nix
        cd $PRJ_ROOT
      '';
    };

    # FIXME: handle sudo for `nixos-rebuild switch` etc.
    rebuildSysCmd = op: ''
      ${withTermFix} ${withCachixToken} ${cachixExec} \
        ${rebuildSystem} -- ${op} --flake $PRJ_ROOT --verbose
    '';
    do-rebuild-sys = dotfield' {
      name = "do-rebuild-sys";
      help = "run the system's rebuild command with the supplied operation arg";
      command = ''${rebuildSysCmd "$1"}'';
    };

    commonCommands = [
      (dotfield' {
        name = "do-format";
        command = ''
          treefmt --clear-cache -- "$@"
        '';
      })

      (dotfield terraform)

      # FIXME: non-functional, needs re-work
      (dotfield' {
        name = "do-repl";
        help = "a REPL for the system flake, by flake-utils-plus";
        command = "${getExe fup-repl} $@";
      })

      (dotfield' {
        name = "do-theme-kitty";
        help = "change the current kitty theme";
        command = ''
          ${getExe pkgs.kitty} @set-colors -a -c \
            $KITTY_CONFIG_DIRECTORY/themes/$1.conf
        '';
      })

      do-update-firefox-addons
      do-update-nvfetcher-sources
      do-rebuild-sys

      (dotfield' {
        name = "do-update-all";
        help = "update all of the things and switch to a new generation";
        command = ''
          nix flake update --verbose
          ${do-update-nvfetcher-sources.command}
          ${do-update-firefox-addons.command}
          ${rebuildSysCmd "switch"}
          doom profiles sync
          doom upgrade
        '';
      })

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
