{
  self,
  lib,
  config,
  ...
}:
let
  inherit (lib) mkOption types;

  whoamiSubmodule = types.submodule (
    { options, config, ... }:
    {
      options = {
        firstName = mkOption { type = types.str; };
        lastName = mkOption { type = types.str; };
        name = mkOption {
          type = types.str;
          default = "${config.firstName} ${config.lastName}";
        };
        email = mkOption { type = types.str; };
        github = mkOption {
          type = with types; nullOr str;
          default = null;
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
        mastodon = mkOption {
          type = with types; nullOr str;
          default = null;
        };
      };
    }
  );

  fontFamilySubmodule = types.submodule {
    options = {
      name = lib.mkOption {
        type = with types; str;
        description = "Font family name";
      };
      pname = lib.mkOption {
        type = with types; str;
        description = "Package name for the font";
      };
    };
  };

  fontSizesSubmodule = types.submodule {
    options = {
      applications = lib.mkOption {
        type = with types; int;
        default = 12;
        description = "Font size for applications";
      };
      desktop = lib.mkOption {
        type = with types; int;
        default = 10;
        description = "Font size for desktop elements";
      };
      popups = lib.mkOption {
        type = with types; int;
        default = 10;
        description = "Font size for popups";
      };
      terminal = lib.mkOption {
        type = with types; int;
        default = 10;
        description = "Font size for terminal";
      };
    };
  };

  waylandSubmodule = types.submodule {
    options = {
      desktop = lib.mkOption {
        type = with types; str;
        default = "hyprland";
        description = "Wayland desktop environment/compositor";
      };
      bar = lib.mkOption {
        type = with types; str;
        default = "waybar";
        description = "Wayland status bar";
      };
      menu = lib.mkOption {
        type = with types; str;
        default = "fuzzel";
        description = "Wayland menu/dmenu replacement";
      };
      launcher = lib.mkOption {
        type = with types; str;
        default = "fuzzel";
        description = "Wayland application launcher";
      };
      notifications = lib.mkOption {
        type = with types; str;
        default = "mako";
        description = "Wayland notification daemon";
      };
    };
  };

  userPreferencesSubmodule = types.submodule {
    options.preferences = {
      editor = lib.mkOption {
        type = with types; str;
        default = "emacsclient";
        description = "Default text editor command";
      };

      term = lib.mkOption {
        type = with types; str;
        default = "ghostty";
        description = "Default terminal emulator";
      };

      shell = lib.mkOption {
        type = with types; str;
        default = "fish";
        description = "Default shell";
      };

      file-manager = lib.mkOption {
        type = with types; str;
        default = "nemo";
        description = "Default file manager";
      };

      audio-player = lib.mkOption {
        type = with types; str;
        default = "mpv";
        description = "Default audio player";
      };

      video-player = lib.mkOption {
        type = with types; str;
        default = "mpv";
        description = "Default video player";
      };

      web-browser = lib.mkOption {
        type = with types; str;
        default = "firefox";
        description = "Default web browser";
      };

      theme = {
        color = {
          variant = lib.mkOption {
            type =
              with types;
              enum [
                "light"
                "dark"
              ];
            default = "light";
            description = "Color theme variant";
          };

          scheme = {
            dark = lib.mkOption {
              type = with types; str;
              default = "catppuccin-mocha";
              description = "Dark color scheme name";
            };

            light = lib.mkOption {
              type = with types; str;
              default = "catppuccin-latte";
              description = "Light color scheme name";
            };
          };
        };

        font = {
          families = {
            sansSerif = lib.mkOption {
              type = fontFamilySubmodule;
              default = {
                name = "Inter";
                pname = "inter";
              };
              description = "Sans-serif font family configuration";
            };

            serif = lib.mkOption {
              type = fontFamilySubmodule;
              default = {
                name = "Aporetic Serif";
                pname = "aporetic";
              };
              description = "Serif font family configuration";
            };

            monospace = lib.mkOption {
              type = fontFamilySubmodule;
              default = {
                name = "Aporetic Sans Mono";
                pname = "aporetic";
              };
              description = "Monospace font family configuration";
            };
          };

          sizes = lib.mkOption {
            type = fontSizesSubmodule;
            default = {
              applications = 12;
              desktop = 10;
              popups = 10;
              terminal = 10;
            };
            description = "Font size configuration";
          };
        };

        icons = lib.mkOption {
          type = types.submodule {
            options = {
              pname = lib.mkOption {
                type = with types; str;
                description = "Icon theme package name";
              };
              dark = lib.mkOption {
                type = with types; str;
                description = "Dark variant icon theme name";
              };
              light = lib.mkOption {
                type = with types; str;
                description = "Light variant icon theme name";
              };
            };
          };
          default = {
            pname = "papirus-icon-theme";
            dark = "Papirus Dark";
            light = "Papirus Light";
          };
          description = "Icon theme configuration";
        };

        wallpaper = lib.mkOption {
          type = types.submodule {
            options = {
              image = lib.mkOption {
                type = with types; nullOr str;
                default = null;
                description = "Path to wallpaper image";
              };
              mode = lib.mkOption {
                type = with types; str;
                default = "fit";
                description = "Wallpaper display mode";
              };
            };
          };
          default = {
            image = null;
            mode = "fit";
          };
          description = "Wallpaper configuration";
        };

        cursor = lib.mkOption {
          type = types.submodule {
            options = {
              name = lib.mkOption {
                type = with types; str;
                description = "Cursor theme name";
              };
              pname = lib.mkOption {
                type = with types; str;
                description = "Cursor theme package name";
              };
              size = lib.mkOption {
                type = with types; int;
                description = "Cursor size";
              };
            };
          };
          default = {
            name = "Bibata-Modern-Classic";
            pname = "bibata-cursors";
            size = 16;
          };
          description = "Cursor theme configuration";
        };

        gui = lib.mkOption {
          type = types.submodule {
            options = {
              name = lib.mkOption {
                type = with types; str;
                description = "GTK theme name";
              };
              pname = lib.mkOption {
                type = with types; str;
                description = "GTK theme package name";
              };
            };
          };
          default = {
            name = "Flat-Remix-GTK-Grey-Light";
            pname = "flat-remix-gtk";
          };
          description = "GTK theme configuration";
        };
      };

      wayland = lib.mkOption {
        type = waylandSubmodule;
        default = {
          desktop = "hyprland";
          bar = "waybar";
          menu = "fuzzel";
          launcher = "fuzzel";
          notifications = "mako";
        };
        description = "Wayland component configuration";
      };
    };
  };
in
{
  options.dotfield.meta.users = lib.mkOption {
    type = types.lazyAttrsOf (
      types.submodule {
        options = {
          preferences = lib.mkOption {
            type = userPreferencesSubmodule;
            # TODO: is this the right way to use the defaults in the submodule?
            default = { };
            description = "User preferences";
          };
          whoami = lib.mkOption {
            type = whoamiSubmodule;
            # TODO: is this the right way to use the defaults in the submodule?
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
        };
      }
    );
    default = { };
    description = "User metadata and preferences configuration";
  };
}
