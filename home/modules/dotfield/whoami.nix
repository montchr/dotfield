{
  config,
  lib,
  ...
}: let
  inherit (lib) mkOption;
  inherit (lib.types) str nullOr;
  cfg = config.dotfield.whoami;
in {
  options.dotfield.whoami = {
    firstName = mkOption {
      type = str;
    };
    lastName = mkOption {
      type = str;
    };
    fullName = mkOption {
      type = str;
      default = "${cfg.firstName} ${cfg.lastName}";
    };
    email = mkOption {
      type = str;
    };
    githubUserName = mkOption {
      type = nullOr str;
      default = null;
    };
    pgpPublicKey = mkOption {
      type = nullOr str;
      default = null;
    };
  };

  config = {
    # FIXME: set these per user, not for all users...
    dotfield.whoami = {
      firstName = "Chris";
      lastName = "Montgomery";
      email = "chris@cdom.io";
      githubUserName = "montchr";
      pgpPublicKey = "0x135EEDD0F71934F3";
    };
  };
}
