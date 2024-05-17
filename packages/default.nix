{ inputs, ... }:
{
  perSystem =
    { pkgs, system, ... }:
    let
      inherit (pkgs) callPackage;
    in
    {
      packages = inputs.flake-utils.lib.filterPackages system ({
        base16-schemes = callPackage ./base16-schemes/package.nix { };
        ddi = callPackage ./ddi/package.nix { };
        fzf-tab-completion = callPackage ./fzf-tab-completion/package.nix { };
        git-repo-manager = callPackage ./git-repo-manager/package.nix { };
        igr = callPackage ./igr/package.nix { };
        tinty = callPackage ./tinty/package.nix { };
        tomlfmt = callPackage ./tomlfmt/package.nix { };

        # TODO: not ready
        # wp-cli = callPackage ./wp-cli/package.nix { };

        vscode-php-debug = callPackage ./vscode-php-debug/package.nix { };

        ##: fish shell plugins
        fish-plugin-fifc = callPackage ./fish-plugin-fifc/package.nix { };

        ##: gh cli extensions
        gh-i = callPackage ./gh/gh-i/package.nix { };
        gh-repo-explore = callPackage ./gh/gh-repo-explore/package.nix { };
        gh-s = callPackage ./gh/gh-s/package.nix { };

        ##: fonts
        berkeley-mono = callPackage ./berkeley-mono/package.nix { };
        sf-pro = callPackage ./sf-pro/package.nix { };
      });
    };
}
