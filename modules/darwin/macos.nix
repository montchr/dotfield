{ pkgs, lib, config, ... }:

let
  cfg = config.my.modules.macos;
  configDir = config.dotfield.configDir;
in
{
  imports = [
    ./hammerspoon.nix
    # ./security/pam.nix
    ./skhd.nix
    ./yabai.nix
  ];

  options = with lib; {
    my.modules.macos = {
      enable = mkEnableOption ''
        Whether to enable macOS module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      environment = {
        systemPackages = with pkgs; [ mas ];
        variables = {
          # FIXME: this isn't unique to macos and is duplicated in the kitty module
          LANG = "en_US.UTF-8";
        };
      };

      my.modules = {
        hammerspoon.enable = true;
        skhd.enable = true;
        yabai.enable = true;
      };

      my.hm.xdg.configFile = {
        "drafts" = {
          source = "${configDir}/drafts";
          recursive = true;
        };

        "karabiner/karabiner.json" = with config.my.hm.lib.file; {
          source = mkOutOfStoreSymlink
            "${config.dotfield.path}/config/karabiner/karabiner.json";
        };
      };

      my.user.packages = with pkgs;
        [
          (writeScriptBin "toggle-dark-mode"
            (builtins.readFile "${configDir}/darwin/bin/toggle-dark-mode"))
        ];

      # https://github.com/LnL7/nix-darwin/pull/228
      # TODO: errors on activation!
      # security.pam.enableSudoTouchIdAuth = true;

      system = {
        defaults = {
          # TODO: why disabled? caused an error?
          # ".GlobalPreferences".com.apple.sound.beep.sound = "Funk";

          smb = {
            NetBIOSName = config.networking.hostName;
            ServerDescription = config.networking.hostName;
          };

          NSGlobalDomain = {
            AppleFontSmoothing = 0;
            AppleKeyboardUIMode = 3;
            AppleMeasurementUnits = "Inches";
            AppleMetricUnits = 0;
            ApplePressAndHoldEnabled = false;
            AppleShowAllExtensions = true;
            AppleShowScrollBars = "Automatic";
            AppleTemperatureUnit = "Fahrenheit";
            InitialKeyRepeat = 10;
            KeyRepeat = 2;
            NSAutomaticCapitalizationEnabled = false;
            NSAutomaticDashSubstitutionEnabled = false;
            NSAutomaticPeriodSubstitutionEnabled = false;
            NSAutomaticQuoteSubstitutionEnabled = false;
            NSAutomaticSpellingCorrectionEnabled = false;
            # NSDisableAutomaticTermination = null;
            NSDocumentSaveNewDocumentsToCloud = false;
            NSNavPanelExpandedStateForSaveMode = true;
            NSNavPanelExpandedStateForSaveMode2 = true;
            NSScrollAnimationEnabled = true;
            NSTableViewDefaultSizeMode = 1;
            NSTextShowsControlCharacters = true;
            # Disable the over-the-top focus ring animation
            # NSUseAnimatedFocusRing = false;
            NSWindowResizeTime = "0.001";
            PMPrintingExpandedStateForPrint = true;
            PMPrintingExpandedStateForPrint2 = true;
            # Whether to autohide the menu bar.  The default is false.
            # _HIHideMenuBar = true;
            # Use F1, F2, etc. keys as standard function keys.
            "com.apple.keyboard.fnState" = false;
            # Configures the trackpad tap behavior. Mode 1 enables tap to click.
            "com.apple.mouse.tapBehavior" = 1;
            # Apple menu > System Preferences > Sound Make a feedback sound when
            # the system volume changed. This setting accepts the integers 0 or
            # 1. Defaults to 1.
            "com.apple.sound.beep.feedback" = 0;
            # https://daiderd.com/nix-darwin/manual/index.html#opt-system.defaults.NSGlobalDomain.com.apple.sound.beep.volume
            # "com.apple.sound.beep.volume" = null;
            # Set the spring loading delay for directories. The default is the float `1.0`.
            "com.apple.springing.delay" = "0.1";
            # Whether to enable spring loading (expose) for directories.
            "com.apple.springing.enabled" = true;
            # Whether to enable "Natural" scrolling direction. The default is true.
            "com.apple.swipescrolldirection" = false;
            # Whether to enable trackpad secondary click. The default is true.
            # "com.apple.trackpad.enableSecondaryClick" = true;
            # Configures the trackpad tracking speed (0 to 3). The default is "1".
            # "com.apple.trackpad.scaling" = "1.0";
            # Configures the trackpad corner click behavior. Mode 1 enables right click.
            # https://daiderd.com/nix-darwin/manual/index.html#opt-system.defaults.NSGlobalDomain.com.apple.trackpad.trackpadCornerClickBehavior
            # "com.apple.trackpad.trackpadCornerClickBehavior" = null;
          };

          # Prevent incessant nagging when opening downloaded apps.
          LaunchServices.LSQuarantine = false;
          # Keep macOS up to date.
          SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;

          # Firewall
          alf = {
            allowdownloadsignedenabled = 0;
            allowsignedenabled = 1;
            globalstate = 0;
            loggingenabled = 0;
            stealthenabled = 0;
          };

          dock = {
            autohide = true;
            autohide-delay = "0";
            autohide-time-modifier = "0";
            dashboard-in-overlay = false;
            enable-spring-load-actions-on-all-items = false;
            expose-animation-duration = "0.1";
            expose-group-by-app = false;
            launchanim = false;
            mineffect = "genie";
            minimize-to-application = true;
            mouse-over-hilite-stack = true;
            mru-spaces = false;
            orientation = "left";
            show-process-indicators = true;
            show-recents = false;
            showhidden = true;
            static-only = true;
            tilesize = 32;
          };

          finder = {
            AppleShowAllExtensions = true;
            # QuitMenuItem = true;
            FXEnableExtensionChangeWarning = false;
            QuitMenuItem = false;
            _FXShowPosixPathInTitle = false; # In Big Sur this is so UGLY!
          };

          loginwindow = {
            DisableConsoleAccess = false;
            GuestEnabled = false;
            # Text to be shown on the login window. Default "\\U03bb".
            # LoginwindowText = null;
            PowerOffDisabledWhileLoggedIn = false;
            RestartDisabled = false;
            RestartDisabledWhileLoggedIn = false;
            SHOWFULLNAME = false;
            ShutDownDisabled = false;
            ShutDownDisabledWhileLoggedIn = false;
            SleepDisabled = true;
            # Documenting that this option exists, but we shouldn't use it.
            # https://daiderd.com/nix-darwin/manual/index.html#opt-system.defaults.loginwindow.autoLoginUser
            # autoLoginUser = false;
          };

          # The filesystem path to which screencaptures should be written.
          # screencapture.location = null;

          spaces = {
            # spans-displays: true;
          };

          trackpad = {
            # 0 to enable Silent Clicking, 1 to disable. The default is 1.
            ActuationStrength = 1;
            # Whether to enable trackpad tap to click. The default is false.
            Clicking = true;
            # For normal click: 0 for light clicking, 1 for medium, 2 for firm. The default is 1.
            FirstClickThreshold = 1;
            # For force touch: 0 for light clicking, 1 for medium, 2 for firm. The default is 1.
            SecondClickThreshold = 1;
            TrackpadRightClick = true;
            TrackpadThreeFingerDrag = true;
          };
        };

        keyboard = {
          enableKeyMapping = true;
          # Whether to remap the Tilde key on non-us keyboards.
          # TODO: `nix build` says this doesn't exist!
          # remapTilde = false;
          remapCapsLockToControl = true;
          # remapCapsLockToEscape = false;
        };

      };
    };
}
