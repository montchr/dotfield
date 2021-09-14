{ config, lib, pkgs, inputs, ... }:

let
  inherit (inputs) agenix;
  inherit (lib)
    mkEnableOption
    mkIf
    ;

  cfg = config.my.modules.secrets;
in
{
  options = {
    my.modules.secrets = {
      enable = mkEnableOption false;
    };
  };

  config = mkIf cfg.enable {
    age.secrets = {
      testSecret.file = ./testSecret.txt;
    };

    age.sshKeyPaths =
      let dir = "${config.my.user.home}/.ssh"; in
      [
        "${dir}/id_ed25519_yubikey"
        "${dir}/id_ed25519"
      ];

    environment.systemPackages =
      let inherit (pkgs) age rage; in
      [
        age
        agenix.defaultPackage."x86_64-darwin"
        rage
      ];
  };
}
