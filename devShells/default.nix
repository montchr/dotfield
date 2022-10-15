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
    inherit (inputs'.deadnix.packages) deadnix;
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
      manix
      nix-diff
      nix-tree
      nvd
      nvfetcher
      rage
      shellcheck
      ssh-to-age
      statix
      sops
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
    linters' = withCategory "linters";
    formatters = pkgWithCategory "formatters";
    formatters' = withCategory "formatters";
    utils = pkgWithCategory "utils";
    utils' = withCategory "utils";
    secrets = pkgWithCategory "secrets";
    secrets' = withCategory "secrets";

    # Mitigate issues with home-manager and kitty terminal on Darwin
    withTermFix = optionalString isDarwin ''TERM="xterm-256color"'';

    cacheName = "dotfield";
    withCachixToken = ''CACHIX_AUTH_TOKEN="$(${getExe pkgs.pass} show cachix.org/${cacheName}/auth-token)"'';
    cachixExec = ''${getExe cachix} watch-exec --jobs 2 ${cacheName}'';

    # FIXME: non-functional, needs re-work
    # do-repl =
    #   dotfield' {
    #     name = "do-repl";
    #     help = "a REPL for the system flake, by flake-utils-plus";
    #     command = "${getExe fup-repl} $@";
    #   };

    do-theme-kitty = dotfield' {
      name = "do-theme-kitty";
      help = "change the current kitty theme";
      command = ''
        ${getExe pkgs.kitty} @set-colors -a -c \
          $KITTY_CONFIG_DIRECTORY/themes/$1.conf
      '';
    };

    do-theme-emacs = dotfield' {
      name = "do-theme-emacs";
      help = "change the current emacs theme";
      command = ''
        emacsclient --no-wait --eval "(modus-themes-toggle)" >/dev/null
      '';
    };

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

    do-update-all = dotfield' {
      name = "do-update-all";
      help = "update all of the things and build a new system derivation";
      command = ''
        set -e
        nix flake update --verbose
        ${do-update-nvfetcher-sources.command}
        ${do-update-firefox-addons.command}
        doom upgrade
        ${do-sys-rebuild.command} build
        ${do-sys-diff-next.command}
      '';
    };

    do-sys-rebuild = dotfield' {
      name = "do-rebuild-sys";
      help = "run the system's rebuild command with the supplied operation arg";
      command = ''
        set -e
        ${withTermFix} ${withCachixToken} ${cachixExec} \
          ${rebuildSystem} -- $@ --flake $PRJ_ROOT --verbose
      '';
    };

    do-sys-diff-next = utils' {
      name = "do-sys-diff-next";
      help = "compare the current system derivation with a project-local result";
      command = ''
        ${getExe nvd} diff /run/current-system $PRJ_ROOT/result
      '';
    };

    do-sys-next = dotfield' {
      name = "do-sys-next";
      help = "switch to a new system generation";
      command = ''
        ${do-sys-rebuild.command} build
        ${do-sys-diff-next.command}
        sudo ${do-sys-rebuild.command} switch
      '';
    };

    commonCommands = [
      do-update-firefox-addons
      do-update-nvfetcher-sources
      do-update-all
      do-sys-diff-next
      do-sys-next
      do-sys-rebuild

      do-theme-kitty
      do-theme-emacs

      (utils cachix)
      (utils manix)
      (utils nix-diff)
      (utils nix-tree)

      (linters editorconfig-checker)
      (linters shellcheck)
      (linters statix)

      # TODO: ignore generated files, contributed modules, etc.
      # (tho rly contrib modules should live elsewhere)
      (linters' {
        name = "deadnix-check";
        help = "run deadnix linting on project nix files";
        command = ''
          ${getExe deadnix} check \
            --no-underscore \
            --fail \
            --no-lambda-arg \
            --no-lambda-pattern-names \
            $PRJ_ROOT
        '';
      })
      (linters' {
        name = "evalnix";
        help = "check for nix syntax errors";
        command = ''
          ${getExe pkgs.fd} --extension nix --exec \
            nix-instantiate --parse --quiet {} >/dev/null
        '';
      })

      (formatters alejandra)
      (formatters prettier)
      (formatters treefmt)
      (formatters' {
        name = "do-format";
        help = "run treefmt on the project";
        command = ''${getExe treefmt} --clear-cache -- "$@"'';
      })

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
        deadnix
        sops-import-keys-hook
        sops-init-gpg-key
        ssh-to-pgp
      ];
    });
  };
}
