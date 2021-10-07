{ config, pkgs, lib, home-manager, options, ... }:

with lib;

let
  mkOptStr = value:
    mkOption {
      type = with types; uniq str;
      default = value;
    };

  mkSecret = description: default:
    mkOption {
      inherit description default;
      type = with types; either str (listOf str);
    };

  mkOpt = type: default: mkOption { inherit type default; };
  mkOpt' = type: default: description:
    mkOption { inherit type default description; };

  mkBoolOpt = default:
    mkOption {
      inherit default;
      type = types.bool;
      example = true;
    };
in
{
  options = with types; {
    dotfield = let t = either str path; in
      rec {
        configDir = mkOpt t "${config.dotfield.dir}/config";
        dir = mkOpt t (toString ../.);
        # FIXME: This points to an arbitrary location which may vary per system.
        # Instead, it should be determined programmatically based on the flake's
        # actual location. Note, however, that its currently only useful for
        # out-of-store symlinks, which are generally discouraged as they do not
        # adhere to the Nix principle of immutable configuration.
        path = mkOpt t "${config.my.user.home}/.config/dotfield";
        binDir = mkOpt t "${config.dotfield.dir}/bin";
        libDir = mkOpt t "${config.dotfield.dir}/lib";
        modulesDir = mkOpt t "${config.dotfield.dir}/modules";
        vendorDir = mkOpt t "${config.dotfield.dir}/vendor";
      };

    my = {
      name = mkOptStr "Chris Montgomery";
      timezone = mkOptStr "America/New_York";
      username = mkOptStr "montchr";
      website = mkOptStr "https://github.com/montchr";
      githubUsername = mkOptStr "montchr";
      email = mkOptStr "chris@cdom.io";
      terminal = mkOptStr "kitty";
      nix_managed = mkOptStr
        "DO NOT EDIT! - managed by Nix - see source inside ${config.dotfield.dir}";
      user = mkOption { type = options.users.users.type.functor.wrapped; };

      keys = {
        pgp = mkOptStr "0x135EEDD0F71934F3";
        ssh = mkOpt (listOf string) [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3tWcOwvNOfHXX3YvtLmJRigxATUh++bWRCAM07uy3mbNvEteT5bF/7nixO44gep0Hv24jaqLeGjCaTxFXrmt1NGgvmAXcsoS4I3+N2xfiFZPIKoiF0EONDsInjm4h5eNoPPE4Rd9xLju4S4tXaXDcL37PunQZJ+aR6CRVf/geM+H4y70cvYHV6uakMAfuv/0+AEMLwlSIN7OpDN8B+JGI4rQhBsekRkkkcZlPYO4vT63aTvLCYFxJ/fR45oMKW57lvZUrbRMHbKRkOfyhBF3qbYR/9aMEUd7gjYBfLJ1hQaHlp2aV49m53WFBjmjqjFcxDPxS/HMk/Hazowkw0G6iNzSNHnO5wI/BxIEahavYvd4VOQXpaWs/G58t8kdQol8WFufLjAReP0j16TqcWEHwy1ktMcrpYfDlLSlNcuaUeXJNIyvD3WmfRDXBnxlBenFIqe9lnK8RUVCcxM+lEEJbMWs1ZuWmgXjbt3UkFhSKSv2Adlm2/OfBBCyO46hVmhLfkwzB69aXYqUjPthlvtCDuLxrmT+DZeWsucUKPp2L9PXS6LpbpnIWCqmnGIPLjHBX2X3EOKwrtLAGN5wv7zLv88qHOD0MET2KVZkfTLg04FkcNowNwAlQ8xBBjpt6xEWNFMH532ZRO1CT0VTUNB7nEW2JET1SULsRT/bTUbKQHQ== chris@cdom.io"
        ];
      };

      hm = mkOption { type = options.home-manager.users.type.functor.wrapped; };

      xdg = let inherit (config.my.user) home; in
        {
          bin = mkOpt path "${home}/.local/bin";
          cache = mkOpt path "${home}/.cache";
          config = mkOpt path "${home}/.config";
          data = mkOpt path "${home}/.local/share";
          lib = mkOpt path "${home}/.local/lib";
        };

      env = mkOption {
        type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
        apply = mapAttrs (n: v:
          if isList v then
            concatMapStringsSep ":" (x: toString x) v
          else
            (toString v));
        default = { };
        description = "Environment variables.";
      };
    };
  };

  config = {
    users.users.${config.my.username} = mkAliasDefinitions options.my.user;

    my.user = {
      home =
        if pkgs.stdenv.isDarwin then
          "/Users/${config.my.username}"
        else
          "/home/${config.my.username}";
    };

    my.env = {
      GITHUB_USER = config.my.githubUsername;
      LESSHISTFILE = "$XDG_CACHE_HOME/lesshst";
      WGETRC = "$XDG_CONFIG_HOME/wgetrc";
    };

    environment = {
      variables = with config.my; {
        # `$DOTFIELD_DIR` must point to its absolute path on the system -- not
        # to its location in the Nix store. ZSH may cache a path to an old
        # derivation.
        DOTFIELD_DIR = config.dotfield.path;

        # If `$DOTFIELD_HOSTNAME` matches `$HOSTNAME`, then we can assume the
        # system has been successfully provisioned with Nix. Otherwise,
        # `$DOTFIELD_HOSTNAME` should remain an empty string.
        DOTFIELD_HOSTNAME = config.networking.hostName;

        XDG_BIN_HOME = "${xdg.bin}";
        XDG_CACHE_HOME = "${xdg.cache}";
        XDG_CONFIG_HOME = "${xdg.config}";
        XDG_DATA_HOME = "${xdg.data}";
      };
    };

    my.hm.home = {
      # Necessary for home-manager to work with flakes, otherwise it will
      # look for a nixpkgs channel.
      stateVersion =
        if pkgs.stdenv.isDarwin then
          "21.11"
        else
          config.system.stateVersion;
      username = config.my.username;
    };

    my.hm.xdg.enable = true;

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      users.${config.my.username} = mkAliasDefinitions options.my.hm;
    };

    environment.extraInit = concatStringsSep "\n"
      (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.my.env);
  };
}
