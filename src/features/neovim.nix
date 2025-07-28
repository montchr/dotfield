{
  dotfield.modules.development.home =
    { config, pkgs, ... }:
    let
      cfg = config.programs.neovim;
      dataPath = "${config.xdg.dataHome}/nvim";
      cachePath = "${config.xdg.cacheHome}/nvim";
    in
    {

      home.sessionVariables = {
        NVIM_CACHE_DIR = cachePath;
        NVIM_CONFIG_DIR = "${config.xdg.configHome}/nvim";
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

        extraPackages = [
          pkgs.fd
          pkgs.git
          pkgs.glow # for glow-nvim
          pkgs.ripgrep
          # FIXME: build fails on `aarch64-darwin` when included in `extraPackages`,
          #        but builds fine on its own
          # pkgs.sumneko-lua-language-server
        ];

        plugins = with pkgs.vimPlugins; [
          nvim-lspconfig
          nvim-lsputils
          plenary-nvim
          vim-nix
        ];
      };

      home.packages = [
        (pkgs.writeShellScriptBin "nvim-purge-state" ''
          rm -rf "${cachePath}" "${dataPath}"
        '')
      ];

    };
}
