{
  config,
  pkgs,
  lib,
  flake,
  ...
}:
let
  inherit (config.home) username sessionVariables;
  l = flake.inputs.nixpkgs.lib // builtins;

  ezaCfg = config.programs.eza;

  # FIXME: hardcoded -- note also that we can't rely on variable expansion to resolve paths in env.nu
  userConfigDir = "~/.config/dotfield/users/${username}/config/nushell";

  /*
    @partial
    replaceVars :: [String] -> (String -> String)
  */
  replaceVars = names: l.replaceStrings (l.map (v: "$" + v) names) (l.map (v: "$env.${v}") names);

  # replaceVars' :: String -> String
  replaceVars' = replaceVars commonNames;

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

  # attrsToEnvDecls :: { n :: String } -> String
  attrsToEnvDecls =
    attrs:
    lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        name: value:
        let
          value' = replaceVars' value;
        in
        "$env.${name} = `${value'}`"
      ) attrs
    );
in
{
  imports = [ ../common.nix ];

  # TODO: install these via module flags (needs dev)
  home.packages = [
    pkgs.fish # for its completion engine, nothing more
    pkgs.jc
  ];

  programs.nushell = {
    enable = true;
    extraConfig = ''
      source ${userConfigDir}/config.nu
    '';
    extraEnv = ''
      source ${userConfigDir}/env.nu
    '';
  };

  xdg.configFile."nushell/home.nu".source = pkgs.writeText "home.nu" ''
    ${attrsToEnvDecls sessionVariables}
  '';

  # FIXME: needs PR to upstream `eza` module for fix to use
  # `home.shellAliases`, not shell-specific options
  home.shellAliases = lib.mkIf (ezaCfg.enable && ezaCfg.enableAliases) {
    ls = "eza";
    ll = "eza -l";
    la = "eza -a";
    lt = "eza --tree";
    lla = "eza -la";
  };
}
