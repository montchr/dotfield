{ config, pkgs, lib, home-manager, options, ... }:

let
  inherit (lib) types;
  inherit (pkgs.lib.our) mkOpt mkOpt' mkBoolOpt;
  inherit (config.my.user) home;

  sshDir = "${home}/.ssh";
in

{
  options = with types; {

    my = {
      name = mkOpt str "Chris Montgomery";
      timezone = mkOpt str "America/New_York";
      username = mkOpt str "montchr";
      website = mkOpt str "https://github.com/montchr";
      githubUsername = mkOpt str "montchr";
      email = mkOpt str "chris@cdom.io";
      terminal = mkOpt str "kitty";
      nix_managed = mkOpt str
        "DO NOT EDIT! - managed by Nix - see source inside ${config.dotfield.dir}";
      user = lib.mkOption { type = options.users.users.type.functor.wrapped; };

      keys = {
        pgp = mkOpt str "0x135EEDD0F71934F3";
        ssh = mkOpt (listOf string) (import ./ssh-keys.nix);
        # FIXME: rename key to standard name
        sshKeyPath = mkOpt path "${sshDir}/id_ed25519_yubikey.pub";
      };

      hm = lib.mkOption {
        description = "Primary user's home-manager configuration";
        type = types.attrs;
        default = { };
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
        type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
        apply = lib.mapAttrs (n: v:
          if lib.isList v then
            lib.concatMapStringsSep ":" (x: toString x) v
          else
            (toString v));
        default = { };
      };
    };
  };
}
