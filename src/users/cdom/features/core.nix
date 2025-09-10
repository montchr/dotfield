{ config, ... }:
let
  inherit (config.meta.users.cdom) whoami;
in
{
  users.cdom.aspects.core.home =
    { pkgs, ... }:
    {
      programs.ghostty.settings.command = "fish";

      programs.git.signing.key = whoami.pgp.id;
      services.gpg-agent = {
        enableSshSupport = true;
        enableExtraSocket = true;
      };
    };
}
