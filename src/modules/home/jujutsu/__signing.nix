{ meta }:
{ lib, config, ... }:
let
  inherit (lib) types;
  inherit (meta.users.${config.home.username}) whoami;

  cfg = config.programs.jujutsu;
in
{
  options.programs.jujutsu = {
    signing = {
      gpg = {
        enable = lib.mkEnableOption "commit signing with GnuPG";
      };
      onPush = lib.mkOption {
        type = types.bool;
        default = true;
        description = "Defer commit signing until pushing to a remote";
      };
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      (lib.mkIf cfg.signing.gpg.enable {
        programs.jujutsu.settings.signing = {
          behavior = "own";
          backend = "gpg";
          key = whoami.pgp.id;
        };
      })
      (lib.mkIf cfg.signing.onPush {
        # Defer signing until push to remote.  This provides a very
        # significant performance boost during iteration due to the
        # increased ephemerality of git commits using the git backend.
        programs.jujutsu.settings.git.sign-on-push = true;

        # Drop signatures on commit modification.  Commits will be re-signed
        # if necessary upon push.
        programs.jujutsu.settings.signing.behavior = "drop";
      })
    ]
  );
}
