# SPDX-FileCopyrightText: 2023-2024 Chris Montgomery <chmont@proton.me>
# SPDX-License-Identifier: GPL-3.0-or-later
# TODO: https://github.com/ncfavier/config/blob/667516e2ea95a0f6604290f1c27f3bd79bc909bd/modules/nix.nix#L11-L15
moduleArgs@{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (config.home) homeDirectory username;
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;

  cfg = config.dotfield;

  identity = import "${flake.path}/users/${config.username}/identity.nix";
in
{
  options.dotfield = {
    features = {
      desktop = {
        enable = mkEnableOption "desktop support";
        session = mkOption {
          default = null;
          type = with types; nullOr (enum [ "gnome" ]);
        };
      };
    };
    paths = {
      fsPath = mkOption {
        type = types.str;
        example = "${homeDirectory}/.config/dotfield";
        description = ''
          Absolute filesystem path to the Dotfield user configuration flake.

          This must not be a path inside the Nix store.
        '';
      };
      user = {
        base = mkOption {
          type = with types; str;
          default = "${config.dotfield.paths.fsPath}/users/${config.home.username}";
          defaultText = lib.literalExpression "\${config.dotfield.paths.fsPath}/users/\${config.home.username}";
          description = ''
            Absolute filesystem path to the Dotfield base directory for this user.
          '';
        };
        configs = mkOption {
          type = with types; str;
          default = "${config.dotfield.paths.user.base}/config";
          defaultText = lib.literalExpression "\${config.dotfield.paths.user.base}/config";
          description = ''
            Absolute filesystem path to the Dotfield raw configuration sources for this user.
          '';
        };
        profiles = mkOption {
          type = with types; str;
          default = "${config.dotfield.paths.user.base}/profiles";
          defaultText = lib.literalExpression "\${config.dotfield.paths.user.base}/profiles";
          description = ''
            Absolute filesystem path to the Dotfield user-specific profiles.
          '';
        };
      };
    };
    whoami = {
      firstName = mkOption { type = types.str; };
      lastName = mkOption { type = types.str; };
      fullName = mkOption {
        type = types.str;
        default = "${cfg.whoami.firstName} ${cfg.whoami.lastName}";
      };
      email = mkOption { type = types.str; };
      githubUserName = mkOption {
        type = with types; nullOr str;
        default = null;
      };
      pgp = mkOption {
        type = with types; nullOr str;
        default = null;
      };
    };
  };

  config = lib.mkMerge [
    {
      assertions = [
        {
          assertion = cfg.features.desktop.enable && cfg.features.desktop.session != null;
          message = "Desktop session type must be set.";
        }
      ];

      home.sessionVariables."DOTFIELD_DIR" = cfg.paths.fsPath;
    }

    ({
      dotfield.whoami = {
        inherit (identity)
          firstName
          lastName
          email
          githubUserName
          pgp
          ;
      };

      programs.git = {
        userEmail = cfg.whoami.email;
        userName = cfg.whoami.fullName;
        extraConfig.github.user = cfg.whoami.githubUserName;
      };
    })

    (mkIf (moduleArgs.osConfig.services.xserver.enable or false) {
      dotfield.features.desktop.enable = true;
    })
  ];
}
