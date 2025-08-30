{ flake, config, ... }:
let
  inherit (flake.config.meta.users.${config.home.username}) whoami;
in
{
  programs.git.signing = {
    key = whoami.pgp.id;
    signByDefault = true;
  };
}
