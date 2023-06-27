{
  flake,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = flake.inputs.nixpkgs.lib // builtins;
  plugin = pkg: {inherit (pkg) name src;};
in {
  imports = [../common.nix];

  programs.fish = {
    enable = true;
    plugins = l.map plugin (with pkgs.fishPlugins; [
      autopair
      done
      # TODO: will conflict with our shell aliases, needs configuration
      # forgit
    ]);

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
