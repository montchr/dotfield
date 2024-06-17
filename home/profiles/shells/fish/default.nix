{ lib, pkgs, ... }:
{
  imports = [ ../common.nix ];

  programs.fish = {
    enable = true;
    plugins = map (pkg: { inherit (pkg) name src; }) (
      with pkgs.fishPlugins;
      [
        autopair
        colored-man-pages
        done
      ]
    );
  };

  # Install package-supplied completions.
  home.extraOutputsToInstall = [ "/share/fish" ];

  programs.fzf.enableFishIntegration = true;
  programs.neovim.plugins = [ pkgs.vimPlugins.vim-fish ];
}
