{
  config,
  pkgs,
  lib,
  home-manager,
  options,
  ...
}:
with lib.types; let
  inherit (pkgs.lib.our) mkOpt mkOpt' mkBoolOpt;
  inherit (config.my.user) home;

  sshDir = "${home}/.ssh";
in {
  options = {
    my = {
      name = mkOpt str "Chris Montgomery";
      timezone = mkOpt str "America/New_York";
      username = mkOpt str "montchr";
      website = mkOpt str "https://github.com/montchr";
      email = mkOpt str config.my.emails.personal;

      usernames = lib.mkOption {
        type = types.submodule {
          options = {
            github = mkOpt' str "" "GitHub username";
            gitlab = mkOpt' str "" "GitLab username";
            sourcehut = mkOpt' str "" "Sourcehut username";
          };
        };
        default = {};
      };

      terminal = mkOpt str "kitty";
      nix_managed =
        mkOpt str
        "DO NOT EDIT! - managed by Nix - see source inside ${config.dotfield.dir}";
      user = lib.mkOption {type = options.users.users.type.functor.wrapped;};

      emails = {
        personal = mkOpt str "chris@cdom.io";
        work = mkOpt str "chris@alley.co";
      };

      keys = {
        pgp = mkOpt str "0x135EEDD0F71934F3";
        ssh = lib.mkOption {
          type = types.submodule {
            options = {
              primary = mkOpt str "";
              identities = mkOpt (listOf str) [];
              hostKeyPaths = mkOpt (listOf str) [
                "${sshDir}/id_ed25519"
                # FIXME: filter non-existant paths
                # "${sshDir}/id_rsa"
              ];
            };
          };
        };
      };

      hm = lib.mkOption {
        description = "Primary user's home-manager configuration";
        type = types.attrs;
        default = {};
      };

      xdg = {
        bin = mkOpt path "${home}/.local/bin";
        cache = mkOpt path "${home}/.cache";
        config = mkOpt path "${home}/.config";
        data = mkOpt path "${home}/.local/share";
        lib = mkOpt path "${home}/.local/lib";
        state = mkOpt path "${home}/.local/state";
      };

      env = lib.mkOption {
        description = "Environment variables.";
        type = attrsOf (oneOf [str path (listOf (either str path))]);
        apply = lib.mapAttrs (n: v:
          if lib.isList v
          then lib.concatMapStringsSep ":" (x: toString x) v
          else (toString v));
        default = {};
      };
    };
  };
}
