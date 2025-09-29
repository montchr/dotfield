{ lib, ... }:
let
  inherit (lib) mkOption types;

  emailSubmodule = types.submodule {
    options = {
      primary = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Primary email address for the user";
      };
    };
    freeformType = types.lazyAttrsOf (types.nullOr types.str);
  };

  whoamiSubmodule = types.submodule (
    { options, config, ... }:
    {
      options = {
        firstName = mkOption {
          type = with types; nullOr str;
          description = "Given name for the user";
          default = null;
        };
        lastName = mkOption {
          type = with types; nullOr str;
          description = "Family name for the user";
          default = null;
        };
        name = mkOption {
          type = with types; nullOr str;
          default = null;
        };
        email = mkOption {
          type = emailSubmodule;
          description = "Email addresses for the user";
          default = { };
        };
        accounts = mkOption {
          type = types.submodule accountsSubmodule;
          description = "Service accounts associated with the user";
          default = { };
        };
        pgp = {
          id = mkOption {
            type = with types; nullOr str;
            default = null;
          };
          key = mkOption {
            type = with types; nullOr str;
            default = null;
          };
        };
      };
    }
  );

  accountsSubmodule = {
    options = {
      github = mkOption {
        type = with types; nullOr str;
        default = null;
      };
      mastodon = mkOption {
        type = with types; nullOr str;
        default = null;
      };
      email = mkOption {
        type = types.attrsOf (types.submodule emailInboxSubmodule);
        default = { };
        description = "Email inboxes accessed by the user";
      };
    };
  };

  emailInboxSubmodule = {
    options = {
      primary = mkOption {
        type = types.bool;
        default = false;
        description = "Whether this account should be considered the primary account.";
      };
      localpart = mkOption {
        type = types.str;
        description = "Localpart for this account";
      };
      domain = mkOption {
        type = types.str;
        description = "Domain name for this account";
      };
      provider = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Provider for this account";
      };
      shared = mkOption {
        type = types.bool;
        default = false;
        description = "Whether the inbox is shared with others.";
      };
      alias = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Primary localpart alias for this inbox";
      };
      extraAliases = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Additional localpart aliases associated with this account, in order of preference.";
      };
    };
  };
in
{
  options.meta.users = lib.mkOption {
    type = types.lazyAttrsOf (
      types.submodule (
        { config, ... }:
        {
          options = {
            whoami = lib.mkOption {
              type = whoamiSubmodule;
              default = { };
              description = "User identifying attributes";
            };
            keys.ssh = lib.mkOption {
              type = with types; listOf str;
              default = [ ];
              description = ''
                SSH public keys for the user.

                Keys should be referenced from the centralized key management in src/metadata/data/keys/.
              '';
            };
            noop = lib.mkOption {
              type = types.attrsOf types.anything;
              default = { };
              description = "Placeholder option for non-operational definitions";
            };
          };
        }
      )
    );
    default = { };
    description = "User metadata";
  };
}
