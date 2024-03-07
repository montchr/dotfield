{ flake, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = flake.inputs.nixpkgs.lib // builtins;
  plugin = pkg: { inherit (pkg) name src; };
in
{
  imports = [
    ../common.nix
    ./__fzf-integration.nix
  ];

  programs.fish = {
    enable = true;
    plugins = l.map plugin (
      with pkgs.fishPlugins;
      [
        autopair
        done
        # TODO: will conflict with our shell aliases, needs configuration
        # forgit
      ]
    );

    loginShellInit = l.mkIf isDarwin ''
      # Essential workaround for clobbered `$PATH` with nix-darwin.
      # Without this, both Nix and Homebrew paths are forced to the end of $PATH.
      # <https://github.com/LnL7/nix-darwin/issues/122#issuecomment-1345383219>
      # <https://github.com/LnL7/nix-darwin/issues/122#issuecomment-1030877541>
      #
      # nix-darwin creates the default profile `/nix/var/nix/profiles/default/bin`
      # upon initial installation and thus should be lowest in priority.
      # <https://github.com/LnL7/nix-darwin/issues/655#issuecomment-1551771624>
      #
      # A previous version of this snippet also included:
      #   - /run/wrappers/bin
      #   - /etc/profiles/per-user/$USER/bin
      #
      if test (uname) = Darwin
          fish_add_path --prepend --global \
            "$HOME/.nix-profile/bin" \
            /run/current-system/sw/bin \
            /nix/var/nix/profiles/default/bin \
            /opt/homebrew/bin \
            /opt/homebrew/sbin
      end
    '';
  };

  programs.fzf.enableFishIntegration = l.mkDefault true;
  programs.neovim.plugins = with pkgs.vimPlugins; [ vim-fish ];
}
