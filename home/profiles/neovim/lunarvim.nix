{config, ...}: let
  inherit (config.xdg) dataHome configHome cacheHome;
in {
  home.shellAliases."lvim" = "$HOME/.local/bin/lvim";
  home.sessionVariables = {
    LUNARVIM_RUNTIME_DIR = "${dataHome}/lunarvim";
    LUNARVIM_CONFIG_DIR = "${configHome}/lvim";
    LUNARVIM_CACHE_DIR = "${cacheHome}/lvim";
  };
}
