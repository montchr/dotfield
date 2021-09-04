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

in {
  options = with types; {
    dotfield = let t = either str path;
    in rec {
      configDir = mkOpt t "${config.dotfield.dir}/config";
      dir = mkOpt t (toString ../../.);
      # FIXME: this points to an arbitrary location which will vary per system
      path = mkOpt t "${config.my.user.home}/Developer/dotfield";
      binDir = mkOpt t "${config.dotfield.dir}/bin";
      libDir = mkOpt t "${config.dotfield.dir}/lib";
      modulesDir = mkOpt t "${config.dotfield.dir}/modules";
    };

    my = {
      name = mkOptStr "Chris Montgomery";
      timezone = mkOptStr "America/New_York";
      username = mkOptStr "montchr";
      website = mkOptStr "https://github.com/montchr";
      github_username = mkOptStr "montchr";
      email = mkOptStr "chris@cdom.io";
      key = mkOptStr "0x135EEDD0F71934F3";
      terminal = mkOptStr "kitty";
      nix_managed = mkOptStr
        "DO NOT EDIT! - managed by Nix - see source inside ${config.dotfield.dir}";
      user = mkOption { type = options.users.users.type.functor.wrapped; };

      hm = {
        file = mkOpt' attrs { } "Files to place directly in $HOME";
        configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
        dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
        lib = mkOpt' attrs config.home-manager.users.${config.my.username}.lib
          "home-manager lib alias";
        programs = mkOpt' attrs { } "home-manager programs config";
      };

      xdg = let t = either str path;
      in {
        bin = mkOpt t "$HOME/.local/bin";
        cache = mkOpt t "$HOME/.cache";
        config = mkOpt t "$HOME/.config";
        data = mkOpt t "$HOME/.local/share";
        lib = mkOpt t "$HOME/.local/lib";
      };

      xdgPaths = let home = config.my.user.home;
      in {
        bin = mkOpt str "${home}/.local/bin";
        cache = mkOpt str "${home}/.cache";
        config = mkOpt str "${home}/.config";
        data = mkOpt str "${home}/.local/share";
        lib = mkOpt str "${home}/.local/lib";
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
      home = if pkgs.stdenv.isDarwin then
        "/Users/${config.my.username}"
      else
        "/home/${config.my.username}";

      description = "Primary user account";
    };

    my.env = {
      GITHUB_USER = config.my.github_username;
      LESSHISTFILE = "$XDG_CACHE_HOME/lesshst";
      WGETRC = "$XDG_CONFIG_HOME/wgetrc";
    };

    environment = {
      variables = with config.my; {
        # `$DOTFIELD` must point to its absolute path on the system -- not to
        # its location in the Nix store. ZSH may cache a path to an old
        # derivation.
        # FIXME: replace with DOTFIELD_DIR
        DOTFIELD = config.dotfield.path;

        # FIXME: `$DOTFIELD_DIR` should be preferred going forward, as it's
        # easier to grep. for the time being, it also provides a direct path to
        # the nix configuration within the repo, but that should change once
        # we're ready to move the configuration to the top level.
        DOTFIELD_DIR = config.dotfield.path;

        # If `$DOTFIELD_HOSTNAME` matches `$HOSTNAME`, then we can assume the
        # system has been successfully provisioned with Nix. Otherwise,
        # `$DOTFIELD_HOSTNAME` should remain an empty string.
        DOTFIELD_HOSTNAME = config.networking.hostName;

        XDG_BIN_HOME = "${xdg.bin}";
        XDG_CACHE_HOME = "${xdg.cache}";
        XDG_CONFIG_HOME = "${xdg.config}";
        XDG_DATA_HOME = "${xdg.data}";
        XDG_LIB_HOME = "${xdg.lib}";
      };
    };

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;

      users.${config.my.username} = {
        home = {
          # Necessary for home-manager to work with flakes, otherwise it will
          # look for a nixpkgs channel.
          stateVersion = if pkgs.stdenv.isDarwin then
            "21.11"
          else
            config.system.stateVersion;
          username = config.my.username;
          file = mkAliasDefinitions options.my.hm.file;
        };

        xdg = {
          enable = true;
          configFile = mkAliasDefinitions options.my.hm.configFile;
          dataFile = mkAliasDefinitions options.my.hm.dataFile;
        };

        programs = mkAliasDefinitions options.my.hm.programs;
      };
    };

    environment.extraInit = concatStringsSep "\n"
      (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.my.env);
  };
}
