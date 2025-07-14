{ config, ... }:
let
  inherit (config.dotfield) whoami;
in
{
  programs.jujutsu.settings.signing = {
    behavior = "own";
    backend = "gpg";
    key = whoami.pgp;
  };
}
