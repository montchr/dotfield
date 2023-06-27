{
  flake,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  imports = [../common.nix];

  programs.fish = {
    enable = true;
    # FIXME: building the system configuration...
    # error: The option `home-manager.users.anomich.programs.fish.plugins."[definition 1-entry 1]".__ignoreNulls' does not exist. Definition values:
    #       - In `/nix/store/lyjq6l79hwahj10mdjgw1pw56ib8fwfi-source/profiles/homeProfiles/shells/fish/default.nix': true
    # plugins = with pkgs.fishPlugins; [
    #   done
    #   forgit
    # ];

    # Workaround for clobbered `$PATH` with nix-darwin
    # https://github.com/LnL7/nix-darwin/issues/122#issuecomment-1030877541
    loginShellInit = l.mkIf isDarwin ''
      fish_add_path --move --prepend --path \
        $HOME/.nix-profile/bin \
        /run/wrappers/bin \
        /etc/profiles/per-user/$USER/bin \
        /nix/var/nix/profiles/default/bin \
        /run/current-system/sw/bin \
        /opt/homebrew/bin \
        /opt/homebrew/sbin
    '';
  };

  programs.fzf.enableFishIntegration = l.mkDefault true;
  programs.neovim.plugins = with pkgs.vimPlugins; [vim-fish];
}
