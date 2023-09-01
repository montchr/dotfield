{
  config,
  pkgs,
  lib,
  flake,
  ...
}: let
  inherit (config.home) username sessionVariables;
  l = flake.inputs.nixpkgs.lib // builtins;

  # FIXME: hardcoded
  userConfigDir = "~/.config/dotfield/users/${username}/config/nushell";

  /*
  replaceVars :: [String] -> (String -> String)
  */
  replaceVars = names:
    l.replaceStrings
    (l.map (v: "$" + v) names)
    (l.map (v: "$env.${v}") names);

  commonNames = [
    "HOME"
    "EDITOR"
    "DOTFIELD_DIR"
    "XDG_CONFIG_HOME"
    "XDG_STATE_HOME"
    "XDG_DATA_HOME"
    "XDG_CACHE_HOME"
    "XDG_BIN_HOME"
  ];

  /*
  attrsToEnvDecls :: { n :: String } -> String
  */
  attrsToEnvDecls = attrs:
    lib.concatStringsSep "\n"
    (lib.mapAttrsToList
      (name: value: let
        value' = replaceVars commonNames value;
      in "$env.${name} = `${value'}`")
      attrs);
in {
  imports = [../common.nix];

  # Handle manually.
  programs.direnv.enableNushellIntegration = false;
  # FIXME: broken? zoxide not found. $PATH incomplete
  programs.zoxide.enableNushellIntegration = false;
  programs.nushell = {
    enable = true;
    extraConfig = ''
      source ${userConfigDir}/config.nu
    '';
    extraEnv = ''
      source ${userConfigDir}/env.nu
    '';
    # configFile.source = ./config.nu;
    # envFile.source = ./env.nu;
    # extraConfig = builtins.readFile ./config.nu;
  };
  xdg.configFile."nushell/home.nu".source = pkgs.writeText "home.nu" ''
    ${attrsToEnvDecls sessionVariables}
  '';
}
