moduleArgs@{ config, ... }:
let
  inherit (config.home) homeDirectory;
  osCfg = moduleArgs.osConfig.dotfield or false;
  fsPath = osCfg.paths.fsPath or "${homeDirectory}/.config/dotfield";
in
{
  options.dotfield.features = { };

  config = {
    home.sessionVariables."DOTFIELD_DIR" = fsPath;
  };
}
