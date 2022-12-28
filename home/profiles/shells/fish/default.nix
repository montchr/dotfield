{
  inputs,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = inputs.nixpkgs.lib // builtins;
in {
  imports = [../common.nix];

  programs.fish = {
    enable = true;
    plugins = with pkgs.fishPlugins; [
      done
      forgit
    ];

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

  programs.fzf.enableFishIntegration = true;
  programs.neovim.plugins = with pkgs.vimPlugins; [vim-fish];
}
