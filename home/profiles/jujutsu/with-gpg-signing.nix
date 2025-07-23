{ lib, config, ... }:
let
  inherit (config.dotfield) whoami;
in
{
  programs.jujutsu.settings.signing = {
    behavior = lib.mkDefault "own";
    backend = "gpg";
    key = whoami.pgp;
  };
}
