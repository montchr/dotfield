flake@{ ... }:
{
  aspects.workstation.home =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (flake.config.meta.users.${config.home.username}) whoami;
      key = whoami.pgp.id;
      inherit (pkgs.stdenv.hostPlatform) isDarwin;
    in
    {
      config = lib.mkIf ("" != key) {
        home.sessionVariables.DOTFIELD_PGP_KEY = key;

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
          sshKeys = [ key ];
        };

        programs.gpg = {
          enable = true;
          mutableKeys = false;
          mutableTrust = false;
          publicKeys = [
            {
              text = flake.config.meta.keys.pgp.asc.${key};
              trust = "ultimate";
            }
          ];

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
    };
}
