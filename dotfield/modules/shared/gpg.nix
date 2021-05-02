{ pkgs, lib, config, ... }:

let

  cfg = config.my.modules.gpg;
  cfgDir = "${config.dotfield.configDir}/gpg";

  # TODO: what about pinentry?
  # pinentry = (if stdenv.isDarwin then )

in {
  options = with lib;
    with types; {
      my.modules.gpg = {
        enable = mkEnableOption ''
          Whether to enable gpg module
        '';
      };
    };

  config = with lib;
    mkIf cfg.enable {
      environment.systemPackages = with pkgs; [ gnupg pinentry ];
      my.env = { GNUPGHOME = "$XDG_CONFIG_HOME/gnupg"; };

      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };

      my.hm.configFile = {
        "gnupg/gpg-agent.conf" = {
          text = ''
            # ${config.my.nix_managed}
            default-cache-ttl ${my.modules.gpg.cacheTTL}
            max-cache-ttl 7200
            # TODO: make sure this path is valid
            pinentry-program ${pkgs.pinentry}/bin/pinentry
          '';
        };

        "gnupg/gpg.conf" = {
          text = ''
            # ${config.my.nix_managed}
            use-agent
            keyserver hkps://keys.openpgp.org
          '';
        };
      };
    };
}
