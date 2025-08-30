{
  flake,
  lib,
  config,
  ...
}:
let
  inherit (flake.config.meta.users.${config.home.username}) whoami;
in
{
  programs.jujutsu.settings.signing = {
    behavior = lib.mkDefault "own";
    backend = "gpg";
    key = whoami.pgp.id;
  };
}
