{ config, ... }:
let
  inherit (config.dotfield) whoami;
in
{
  programs.git.signing = {
    key = whoami.pgp;
    signByDefault = true;
  };
}
