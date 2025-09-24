{ config, ... }:
let
  inherit (config.meta) keys;
in
{
  meta.users.cdom.whoami = {
    name = "Chris Montgomery";
    firstName = "Chris";
    lastName = "Montgomery";
    email = "chmont@protonmail.com";
    pgp = rec {
      id = "0x135EEDD0F71934F3";
      # TODO: how can this be made a default?
      key = keys.pgp.asc.${id};
    };
    accounts = {
      github = "montchr";
      mastodon = "@montchr@assemblag.es";
      email = {
        personal = {
          primary = true;
          localpart = "chmont";
          domain = "protonmail.com";
          provider = "proton";
        };
        tu = {
          localpart = "tuc26080";
          alias = "chrismont";
          domain = "temple.edu";
          extraAliases = [ ];
          provider = "outlook";
        };
        kleinweb = {
          localpart = "kleinweb";
          shared = true;
          domain = "temple.edu";
          provider = "outlook";
        };
      };
    };
  };
}
