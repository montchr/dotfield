{ flake, pkgs, ... }:
{
  imports = [ ../common.nix ];

  programs.fish = {
    enable = true;
    plugins =
      with pkgs.fishPlugins;
      [
        autopair
        done
      ]
      |> builtins.map flake.self.lib.fish.toPluginAttrs;
  };

  # Install package-supplied completions.
  home.extraOutputsToInstall = [ "/share/fish" ];

  programs.fzf.enableFishIntegration = true;
  programs.neovim.plugins = [ pkgs.vimPlugins.vim-fish ];
}
