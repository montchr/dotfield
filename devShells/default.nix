{self, ...}: let
  inherit (self) inputs;
in {
  perSystem = {
    inputs',
    system,
    pkgs,
    lib,
    ...
  }: let
    inherit (inputs'.agenix.packages) agenix;
    inherit (inputs'.deploy-rs.packages) deploy-rs;
    # FIXME: does not exist for darwin systems
    # inherit (inputs'.nixos-generators.packages) nixos-generators;
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
      nvfetcher
      rage
      repl
      shellcheck
      shfmt
      ssh-to-age
      sops
      terraform
      treefmt
      ;
    inherit (pkgs.nodePackages) prettier;
    inherit (pkgs.stdenv) isDarwin isLinux;

    withCategory = category: attrset: attrset // {inherit category;};
    pkgWithCategory = category: package: {inherit package category;};

    dotfield = pkgWithCategory "dotfield";
    linter = pkgWithCategory "linters";
    formatter = pkgWithCategory "formatters";
    utils = withCategory "utils";
    secrets = pkgWithCategory "secrets";

    updateSources = {
      category = "dotfield";
      name = nvfetcher.pname;
      help = nvfetcher.meta.description;
      command = "cd $PRJ_ROOT/packages/sources; ${nvfetcher}/bin/nvfetcher -c ./sources.toml $@";
    };

    updateFirefoxAddons = utils {
      name = "mozilla-addons-to-nix";
      help = "Generate a Nix package set of Firefox add-ons from a JSON manifest.";
      # N.B. As a Haskell package, including this flake's default package
      # directly will break our own `nix flake check` due to IFD.
      #
      # The current method here relies on the flake being available in the
      # registry, which happens automatically when added as an input. We
      # could, instead, just as easily reference the flake's upstream URL in
      # the command, but would then lose the benefits of pinning inputs.
      command = "nix run mozilla-addons-to-nix -- $@";
    };
  in {
    devShells.default = inputs'.devshell.legacyPackages.mkShell {
      name = "Dotfield";

      # git.hooks.enable = true;
      # git.hooks.pre-commit.text = ''${pkgs.treefmt}/bin/treefmt'';
      # sopsPGPKeyDirs = ["./nixos/secrets/keys"];
      # sopsCreateGPGHome = true;
      # nativeBuildInputs = [
      #   # inputs'.deploykit.packages.deploykit
      #   # pkgs.python3.pkgs.invoke
      # ];

      commands = [
        (dotfield repl)
        (dotfield deploy-rs)
        # (dotfield nixos-generators)
        (dotfield terraform)
        (dotfield treefmt)

        updateSources
        updateFirefoxAddons

        {
          category = "dotfield";
          name = "dotfield-update-all";
          command = let
            rebuildSystem =
              if isDarwin
              then "darwin-rebuild"
              else "nixos-rebuild";
          in ''
            nix flake update --verbose
            ${updateSources.command}
            ${updateFirefoxAddons.command}
            doom upgrade
            ${rebuildSystem} build --verbose
          '';
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
      ];
    };
  };
}
