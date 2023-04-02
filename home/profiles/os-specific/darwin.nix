{
  config,
  inputs,
  self,
  pkgs,
  darwinConfig,
  ...
}: let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (self.lib.apps.yabai) toYabaiConfig;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = inputs.nixpkgs.lib // builtins;
  configSrcBasePath = "${config.xdg.configHome}/dotfield/home/users/cdom/config";
  yabaiCfg = darwinConfig.services.yabai;
in
  l.mkIf isDarwin {
    xdg.configFile."skhd".source = mkOutOfStoreSymlink "${configSrcBasePath}/skhd";

    xdg.configFile."yabai/yabairc".text = l.concatLines [
      (toYabaiConfig yabaiCfg.config)
      yabaiCfg.extraConfig
    ];
  }
