{
  config,
  lib,
  pkgs,
  ...
}:
let
  passwordStoreDir = config.services.pass-secret-service.storePath;
in
{
  home.packages = [ pkgs.protonmail-bridge-gui ];

  # <https://github.com/NixOS/nixpkgs/issues/126174#issuecomment-1270291680>
  # systemd.user.services.proton-bridge = {
  #   Unit = {
  #     Description = "Proton Bridge";
  #     Requires = [
  #       "pass-secret-service.service"
  #       "gpg-agent.service"
  #     ];
  #   };

  #   Service = {
  #     LogLevel = "debug";
  #     Restart = "always";
  #     ExecStart = "${lib.getExe pkgs.protonmail-bridge} --no-window --noninteractive";
  #     Environment = [
  #       "PATH=${lib.getBin pkgs.gnome-keyring}:${lib.getBin pkgs.pass}"
  #       "PASSWORD_STORE_DIR=${passwordStoreDir}"
  #     ];
  #   };

  #   Install = {
  #     WantedBy = [ "default.target" ];
  #   };
  # };
}
