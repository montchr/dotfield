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
    in {
      # TODO: does this not work?
      # dir = mkOpt t (findFirst pathExists (toString ../.) [
      #   "${config.my.user.home}/.config/dotfield"
      #   "/etc/dotfield"
      # ]);
      dir = mkOpt t "$XDG_CONFIG_HOME/dotfield";
      binDir = mkOpt t "${config.dotfield.dir}/bin";
      configDir = mkOpt t "${config.dotfield.dir}/config";
      modulesDir = mkOpt t "${config.dotfield.dir}/modules";
      themesDir = mkOpt t "${config.dotfield.modulesDir}/themes";
    };

    my = {
      name = mkOptStr "Chris Montgomery";
      timezone = mkOptStr "America/New_York";
      username = mkOptStr "montchr";
      website = mkOptStr "https://cdom.io";
      github_username = mkOptStr "montchr";
      email = mkOptStr "chris@cdom.io";
      terminal = mkOptStr "kitty";
      nix_managed = mkOptStr
        "DO NOT EDIT! - managed by Nix - see source inside ${config.dotfield.dir}";
      user = mkOption { type = options.users.users.type.functor.wrapped; };

      hm = {
        file = mkOpt' attrs { } "Files to place directly in $HOME";
        configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
        dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
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

    environment = {
      variables = {
        DOTFIELD = config.dotfield.dir;
        DOTFIELD_BIN = config.dotfield.binDir;
      };
    };

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;

      #   my.hm.file        ->  home-manager.users.cdom.home.file
      #   my.hm.configFile  ->  home-manager.users.cdom.home.xdg.configFile
      #   my.hm.dataFile    ->  home-manager.users.cdom.home.xdg.dataFile
      #
      # See:
      #   - https://github.com/ahmedelgabri/dotfiles/blob/c2da02d042275ccc476812f79298f0453c2a5263/nix/modules/shared/settings.nix
      #   - https://github.com/hlissner/dotfiles/blob/1173284b76561d41edcb17062badccda012f7f2e/modules/options.nix
      users.${config.my.username} = {
        home = {
          # Necessary for home-manager to work with flakes, otherwise it will
          # look for a nixpkgs channel.
          stateVersion = if pkgs.stdenv.isDarwin then
            "21.05"
          else
            config.system.stateVersion;
          username = config.my.username;
          file = mkAliasDefinitions options.my.hm.file;
        };

        xdg = {
          configFile = mkAliasDefinitions options.my.hm.configFile;
          dataFile = mkAliasDefinitions options.my.hm.dataFile;
        };

        programs = {
          # Let Home Manager install and manage itself.
          home-manager.enable = true;
        };
      };
    };

    environment.extraInit = concatStringsSep "\n"
      (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.my.env);
  };
}
