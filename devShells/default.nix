{...}: {
  perSystem = {
    inputs',
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
    # FIXME: `defaultPackage` doesn't exist? why?
    # nvfetcher = inputs'.nvfetcher.defaultPackage;

    inherit
      (pkgs)
      alejandra
      cachix
      editorconfig-checker
      rage
      shellcheck
      shfmt
      ssh-to-age
      sops
      terraform
      treefmt
      ;
    inherit (pkgs.nodePackages) prettier;
    inherit (pkgs.stdenv) isLinux;

    withCategory = category: attrset: attrset // {inherit category;};
    pkgWithCategory = category: package: {inherit package category;};

    dotfield = pkgWithCategory "dotfield";
    linter = pkgWithCategory "linters";
    formatter = pkgWithCategory "formatters";
    utils = withCategory "utils";
    secrets = pkgWithCategory "secrets";
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
        (dotfield deploy-rs)
        # (dotfield nixos-generators)
        (dotfield terraform)
        (dotfield treefmt)

        # {
        #   category = "dotfield";
        #   name = nvfetcher.pname;
        #   help = nvfetcher.meta.description;
        #   command = "cd $PRJ_ROOT/packages/sources; ${nvfetcher}/bin/nvfetcher -c ./sources.toml $@";
        # }

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
