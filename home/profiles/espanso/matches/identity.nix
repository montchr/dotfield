{ config, ... }:
let
  inherit (config.dotfield) whoami;
in
{
  services.espanso.matches.identity.matches = [
    {
      replace = whoami.pgp;
      trigger = ";me;gpg";
    }
  ];
}
