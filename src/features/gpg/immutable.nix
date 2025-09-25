flake@{ inputs, ... }:
let
  inherit (inputs.apparat.lib) isEmpty;
in
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
    in
    lib.mkIf (!isEmpty key) {
      home.sessionVariables.DOTFIELD_PGP_KEY = key;
      programs.gpg = {
        mutableKeys = false;
        mutableTrust = false;
        publicKeys = [
          {
            text = flake.config.meta.keys.pgp.asc.${key};
            trust = "ultimate";
          }
        ];
      };
      services.gpg-agent = {
        enable = true;
        pinentry.package = pkgs.pinentry-gnome3;
        sshKeys = [ key ];
      };
    };
}
