{ config, lib, ... }:
let
  inherit (config.dotfield) whoami;
in
{
  programs.git.signing = {
    key = whoami.pgp;
    signByDefault = lib.mkDefault true;
  };
}
