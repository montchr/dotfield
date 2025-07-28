{ inputs, ... }:
{
  imports = [
    ./__beets-packages.nix
  ];

  perSystem =
    { pkgs, system, ... }:
    let
      inherit (pkgs) callPackage;
    in
    {
      packages = inputs.flake-utils.lib.filterPackages system {
        astrata = callPackage ./astrata/package.nix { };
        base16-schemes = callPackage ./base16-schemes/package.nix { };
        berkeley-mono = callPackage ./berkeley-mono/package.nix { };
        ddi = callPackage ./ddi/package.nix { };
        fzf-tab-completion = callPackage ./fzf-tab-completion/package.nix { };
        getcert = callPackage ./getcert/package.nix { };
        gh-repo-explore = callPackage ./gh/gh-repo-explore/package.nix { };
        gh-s = callPackage ./gh/gh-s/package.nix { };
        igr = callPackage ./igr/package.nix { };
        iosvmata-bin = callPackage ./iosvmata-bin/package.nix { };
        php-stubs-generator = callPackage ./php-stubs-generator/package.nix { };
        pragmasevka = callPackage ./pragmasevka/package.nix { };
        pragmasevka-bin = callPackage ./pragmasevka-bin/package.nix { };
        realise-symlink = callPackage ./realise-symlink/package.nix { };
        scotty = callPackage ./scotty/package.nix { };
        starship-jj = callPackage ./starship-jj/package.nix { };
        wp-to-psr-4 = callPackage ./wp-to-psr-4/package.nix { };

        ##: yijing tools
        iching = callPackage ./iching/package.nix { };
        yijing-q-merge-csv = callPackage ./yijing-q-merge-csv/package.nix { };

        # TODO: not ready
        # wp-cli = callPackage ./wp-cli/package.nix { };

      };
    };
}
