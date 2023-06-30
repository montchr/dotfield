{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.age;
  secretsDir = ../secrets;
in {
  home-manager.sharedModules = [
    {
      home.sessionVariables = {
        "AGE_KEY_DIR" = lib.mkDefault "$HOME/.age";
      };
    }
  ];

  environment.systemPackages = [
    pkgs.age-plugin-yubikey
    pkgs.rage
  ];

  # TODO: should not specify these manually, but darwin...
  users.groups.secrets.members = ["root" "cdom" "seadoom" "xtallos"];

  age.secrets = {
    # FIXME: must not define at system-level
    # "espanso/personal.yml" = {
    #   file = "${secretsDir}/espanso/personal.yml.age";
    #   path = "${cfg.secretsDir}/espanso/personal.yml";
    # };
  };
}
