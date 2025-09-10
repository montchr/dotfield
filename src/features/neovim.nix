{
  aspects.core.home =
    { config, pkgs, ... }:
    let
      inherit (config) xdg;
      inherit (pkgs) vimPlugins;

      dataPath = "${xdg.dataHome}/nvim";
      cachePath = "${xdg.cacheHome}/nvim";
    in
    {
      home.sessionVariables = {
        NVIM_CACHE_DIR = cachePath;
        NVIM_CONFIG_DIR = "${xdg.configHome}/nvim";
        NVIM_DATA_DIR = dataPath;
      };

      programs.neovim = {
        enable = true;
      };
    };
}
