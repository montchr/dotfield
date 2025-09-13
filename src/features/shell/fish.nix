{
  aspects.core = {
    nixos = {
      programs.fish.enable = true;
    };

    home =
      { pkgs, ... }:
      {
        programs.fish.enable = true;
        programs.fzf.enableFishIntegration = true;
        programs.neovim.plugins = [ pkgs.vimPlugins.vim-fish ];
        home.extraOutputsToInstall = [ "/share/fish" ];
      };
  };

  aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.fish-lsp ];
    };
}
