{ config, ... }:
let
  inherit (config.meta) keys;
  cfg = config.meta.users.cdom.whoami;
in
{
  meta.users.cdom.whoami = {
    name = "Chris Montgomery";
    firstName = "Chris";
    lastName = "Montgomery";
    email = {
      primary = cfg.email.personal;
      personal = "chmont@protonmail.com";
      work = "chrismont@temple.edu";
    };
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
