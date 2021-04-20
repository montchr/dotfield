{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.gpg;
in
{
  options = with lib; {
    my.modules.gpg = {
      enable = mkEnableOption ''
        Whether to enable GPG module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      environment.systemPackages = with pkgs; [ gnupg ];
      my.env = { GNUPGHOME = "$XDG_CONFIG_HOME/gnupg"; };

      # my.user = {
      #   packages = with pkgs;
      #     [
      #       keybase
      #       # keybase-gui # ???
      #     ];
      # };

      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };

      # TODO: why not `my.hm.configFile`?
      my.hm.file = {
        ".config/gnupg/gpg-agent.conf".text = ''
          default-cache-ttl 600
          max-cache-ttl 7200'';

        ".config/gnupg/gpg.conf" = {
          text = ''
            # ${config.my.nix_managed}
            ${builtins.readFile ../../../config/gnupg/gpg.conf}'';
        };
      };
    };
}
