{
  config,
  pkgs,
  inputs,
  ...
}: let
  inherit (config) xdg;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs) vimPlugins;

  # l = inputs.nixpkgs.lib // builtins;
  # lib = import ./lib.nix {inherit inputs;};
  cfg = config.programs.neovim;

  configSrcBasePath = "${xdg.configHome}/dotfield/home/users/cdom/config";
  dataPath = "${xdg.dataHome}/nvim";
  cachePath = "${xdg.cacheHome}/nvim";

  userNamespace = config.home.username;

  initContent = ''
    -- Load custom configuration module
    require("${userNamespace}")
  '';
in {
  imports = [./plugins.nix];

  xdg.configFile."nvim/init.lua".text = (cfg.generatedConfigs.lua or "") + initContent;
  xdg.configFile."nvim/lua/${userNamespace}".source =
    mkOutOfStoreSymlink "${configSrcBasePath}/nvim/lua/${userNamespace}";

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
    withPython3 = false;
    withRuby = false;

    extraPackages = [
      pkgs.fd
      pkgs.git
      pkgs.ripgrep
      pkgs.sumneko-lua-language-server
    ];

    plugins = [
      ###: --- required --------------------------------------------------------

      ##: impatient-nvim :: compile and cache lua modules
      # (lib.makeLuaPlugin' vimPlugins.impatient-nvim ''
      #   require("impatient")
      # '')

      ##: common utilities
      vimPlugins.plenary-nvim
    ];
  };
}
