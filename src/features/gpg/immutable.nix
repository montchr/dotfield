{ lib, self, ... }:
{
  dotfield.modules.workstation.home =
    { config, pkgs, ... }:
    let
      inherit (self.dotfield.meta.users.${config.username}) whoami;
    in
    lib.mkIf ("" != whoami.pgp.id) {
      home.sessionVariables.DOTFIELD_PGP_KEY = whoami.pgp.id;

      programs.gpg = {
        mutableKeys = false;
        mutableTrust = false;
        publicKeys = [
          {
            text = whoami.pgp.key;
            trust = "ultimate";
          }
        ];
      };

      services.gpg-agent.sshKeys = [ whoami.pgp.id ];
    };
}
