{
  users.cdom.aspects.core.home =
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
      xdg.configFile."nvim/init.lua".text = (cfg.generatedConfigs.lua or "") + initContent;

      home.packages = [
        (pkgs.writeShellScriptBin "nvim-purge-state" ''
          rm -rf "${cachePath}" "${dataPath}"
        '')
      ];

      programs.neovim = {
        viAlias = true;
        vimAlias = true;
        vimdiffAlias = true;
        withNodeJs = true;
        withPython3 = true;
        withRuby = true;
      };
    };
}
