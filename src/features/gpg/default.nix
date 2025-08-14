{ lib, self, ... }:
{
  dotfield.features.workstation.home =
    { config, pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isDarwin;
    in
    {
      home.packages = with pkgs; [
        gnupg
        gpgme

        (writeShellScriptBin "gpg-agent-restart" ''
          pkill gpg-agent ; pkill ssh-agent ; pkill pinentry ; eval $(gpg-agent --daemon --enable-ssh-support)
        '')
      ];

      services.gpg-agent = {
        enable = true;
        # FIXME: not necessarily gnome3
        pinentry.package = if isDarwin then pkgs.pinentry_mac else pkgs.pinentry-gnome3;
      };

      programs.gpg = {
        enable = true;

        # https://github.com/drduh/config/blob/master/gpg.conf
        # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Configuration-Options.html
        # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Esoteric-Options.html
        settings = {
          keyserver = "hkps://keys.openpgp.org";
          # keyserver = "hkps://keys.mailvelope.com";
          # keyserver = "hkps://keyserver.ubuntu.com";
        };
      };

    };
}
