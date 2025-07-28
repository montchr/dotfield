{ self, ... }:
{
  dotfield.home =
    { pkgs, ... }:
    {
      programs.fish.enable = true;
      home.extraOutputsToInstall = [ "/share/fish" ];
      programs.fzf.enableFishIntegration = true;
      programs.neovim.plugins = [ pkgs.vimPlugins.vim-fish ];
    };
}
