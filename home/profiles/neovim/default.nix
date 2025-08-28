{ config, pkgs, ... }:
let
  inherit (config) xdg;
  inherit (pkgs) vimPlugins;

  cfg = config.programs.neovim;

  userNamespace = "cdom";
  dataPath = "${xdg.dataHome}/nvim";
  cachePath = "${xdg.cacheHome}/nvim";

  initContent = ''
    -- Load custom configuration module
    -- require("${userNamespace}")
  '';
in
{
  imports = [ ./plugins.nix ];

  xdg.configFile."nvim/init.lua".text = (cfg.generatedConfigs.lua or "") + initContent;

  home.packages = [
    (pkgs.writeShellScriptBin "nvim-purge-state" ''
      rm -rf "${cachePath}" "${dataPath}"
    '')
  ];

  home.sessionVariables = {
    NVIM_CACHE_DIR = cachePath;
    NVIM_CONFIG_DIR = "${xdg.configHome}/nvim";
    NVIM_DATA_DIR = dataPath;
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    plugins = [
      ##: impatient-nvim :: compile and cache lua modules
      # (lib.makeLuaPlugin' vimPlugins.impatient-nvim ''
      #   require("impatient")
      # '')

      ##: common utilities
      vimPlugins.plenary-nvim
    ];
  };
}
