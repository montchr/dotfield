{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (config.home) username;
  inherit (lib) mkOption;
  inherit (lib.types) str nullOr;
  cfg = config.dotfield.whoami;

in
{
  options.dotfield.whoami = {
    firstName = mkOption { type = str; };
    lastName = mkOption { type = str; };
    name = mkOption {
      type = str;
      default = "${cfg.firstName} ${cfg.lastName}";
    };
    email = mkOption { type = str; };
    github = mkOption {
      type = nullOr str;
      default = null;
    };
    pgp = mkOption {
      type = nullOr str;
      default = null;
    };
    mastodon = mkOption {
      type = nullOr str;
      default = null;
    };
  };

  config.dotfield.whoami = import "${flake.self}/users/${username}/whoami.nix";
}
