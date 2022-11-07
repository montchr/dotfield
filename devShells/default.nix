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

    inherit
      (pkgs)
      alejandra
      cachix
      editorconfig-checker
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

    # Mitigate issues with home-manager and kitty terminal on Darwin
    withTermFix = l.optionalString isDarwin ''TERM="xterm-256color"'';

    cacheName = "dotfield";
    cachixExec = ''${l.getExe cachix} watch-exec --jobs 2 ${cacheName}'';

    # FIXME: non-functional, needs re-work
    # do-repl =
    #   dotfield' {
    #     name = "do-repl";
    #     help = "a REPL for the system flake, by flake-utils-plus";
    #     command = "${getExe fup-repl} $@";
    #   };

    # TODO: make argument optional to allow true toggling ... maybe symlinks?
    do-theme-kitty = ui' {
      name = "do-theme-kitty";
      help = "toggle the current kitty theme between light<->dark";
      command = ''
        ${l.getExe pkgs.kitty} @set-colors -a -c \
          $KITTY_CONFIG_DIRECTORY/themes/$1.conf
      '';
    };

    do-theme-emacs = ui' {
      name = "do-theme-emacs";
      help = "toggle the current emacs theme between light<->dark";
      command = ''
        emacsclient --no-wait --eval "(modus-themes-toggle)" >/dev/null
      '';
    };

    do-theme-macos = ui' {
      name = "do-theme-macos";
      help = "toggle the current macOS system theme between light<->dark";
      command = ''
        osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
      '';
    };

    do-theme-toggle = ui' {
      name = "do-theme-toggle";
      help = "toggle all application themes between light<->dark";
      command = ''
        ${l.optionalString isDarwin do-theme-macos.command}
        # FIXME: this command requires light/dark input currently
        # ${do-theme-kitty.command}
        ${do-theme-emacs.command}
      '';
    };

    do-update-nvfetcher-sources = dotfield' {
      name = "do-update-nvfetcher-sources";
      help = nvfetcher.meta.description;
      command = ''
        pushd $PRJ_ROOT/packages/sources
        ${l.getExe nvfetcher} -c ./sources.toml $@
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
        ${withTermFix} ${cachixExec} \
          ${rebuildSystem} -- $@ --flake $PRJ_ROOT --verbose
      '';
    };

    do-sys-diff-next = utils' {
      name = "do-sys-diff-next";
      help = "compare the current system derivation with a project-local result";
      command = ''
        ${l.getExe nvd} diff /run/current-system $PRJ_ROOT/result
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

    commonCommands =
      [
        ##: --- sys/hm mgmt --------------

        do-update-firefox-addons
        do-update-nvfetcher-sources
        do-update-all
        do-sys-diff-next
        do-sys-next
        do-sys-rebuild

        ##: --- ui -----------------------

        do-theme-kitty
        do-theme-emacs
        do-theme-toggle
      ]
      ++ (l.optional isDarwin do-theme-macos)
      ++ [
        ##: --- utils --------------------

        (utils cachix)
        (utils manix)
        (utils nix-diff)
        (utils nix-tree)

        ##: --- linters ------------------

        (linters editorconfig-checker)
        (linters shellcheck)
        (linters statix)

        # TODO: ignore generated files, contributed modules, etc.
        # (tho rly contrib modules should live elsewhere)
        (linters' {
          name = "deadnix-check";
          help = "run deadnix linting on project nix files";
          command = ''
            ${l.getExe deadnix} check \
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
            ${l.getExe pkgs.fd} --extension nix --exec \
              nix-instantiate --parse --quiet {} >/dev/null
          '';
        })

        ##: --- formatters ---------------

        (formatters alejandra)
        (formatters prettier)
        (formatters treefmt)
        (formatters' {
          name = "do-format";
          help = "run treefmt on the project";
          command = ''${l.getExe treefmt} --clear-cache -- "$@"'';
        })

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
        sops-import-keys-hook
        sops-init-gpg-key
        ssh-to-pgp
        nodejs
        yarn
      ];
    });
  };
}
