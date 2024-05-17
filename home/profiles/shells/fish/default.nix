{ lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  plugin = pkg: { inherit (pkg) name src; };
in
{
  imports = [ ../common.nix ];

  programs.fish = {
    enable = true;
    plugins = builtins.map plugin (
      with pkgs.fishPlugins;
      [
        autopair
        done
        # TODO: will conflict with our shell aliases, needs configuration
        # forgit
      ]
    );
  };

  # Install package-supplied completions.
  home.extraOutputsToInstall = [ "/share/fish" ];

  programs.fzf.enableFishIntegration = lib.mkDefault true;
  programs.neovim.plugins = with pkgs.vimPlugins; [ vim-fish ];
}
