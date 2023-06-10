{
  config,
  flake,
  ...
}: let
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  ##: Hostname
  system.defaults.smb.NetBIOSName = config.networking.hostName;
  system.defaults.smb.ServerDescription = config.networking.hostName;

  ###: APPEARANCE ==============================================================

  # `null`  => normal mode
  # `Dark`  => dark mode
  # TODO: use this to change dark mode on the fly with `specialisation`?
  system.defaults.NSGlobalDomain.AppleInterfaceStyle = null;
  # Whether light/dark modes are toggled automatically.
  system.defaults.NSGlobalDomain.AppleInterfaceStyleSwitchesAutomatically = false;
  # Sets the level of font smoothing (sub-pixel font rendering).
  system.defaults.NSGlobalDomain.AppleFontSmoothing = 0;
  system.defaults.universalaccess.reduceTransparency = true;

  ###: SOUND ==================================================================

  # Make a feedback sound when the system volume changed. This setting accepts
  # the integers 0 or 1. Defaults to 1.
  system.defaults.NSGlobalDomain."com.apple.sound.beep.feedback" = 0;
  # 75% => 0.7788008 ; 50% => 0.6065307 ; 25% => 0.4723665
  system.defaults.NSGlobalDomain."com.apple.sound.beep.volume" = null;

  ###: LOCALE ==================================================================

  system.defaults.NSGlobalDomain.AppleMeasurementUnits = "Centimeters";
  system.defaults.NSGlobalDomain.AppleMetricUnits = 1;
  system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;
  system.defaults.NSGlobalDomain.AppleTemperatureUnit = "Fahrenheit";
  system.defaults.NSGlobalDomain.AppleICUForce24HourTime = true;
  system.defaults.menuExtraClock.Show24Hour = true;
  system.defaults.menuExtraClock.ShowAMPM = false;

  ###: UPDATES/SECURITY/FIREWALL ================================================================

  system.defaults.SoftwareUpdate.AutomaticallyInstallMacOSUpdates = true;
  # Prevent nagging when opening downloaded apps.
  system.defaults.LaunchServices.LSQuarantine = false;
  system.defaults.alf.allowdownloadsignedenabled = 0;
  system.defaults.alf.allowsignedenabled = 1;
  system.defaults.alf.globalstate = 0;
  system.defaults.alf.loggingenabled = 0;
  system.defaults.alf.stealthenabled = 0;

  ###: DESKTOPS/SPACES/DOCK ====================================================

  system.defaults.dock.autohide = true;
  system.defaults.dock.autohide-delay = 0.1;
  system.defaults.dock.autohide-time-modifier = 0.1;
  system.defaults.dock.dashboard-in-overlay = false;
  system.defaults.dock.enable-spring-load-actions-on-all-items = false;
  system.defaults.dock.expose-animation-duration = 0.1;
  system.defaults.dock.expose-group-by-app = false;
  system.defaults.dock.launchanim = false;
  system.defaults.dock.mineffect = "genie";
  system.defaults.dock.minimize-to-application = true;
  system.defaults.dock.mouse-over-hilite-stack = true;
  system.defaults.dock.mru-spaces = false;
  system.defaults.dock.orientation = "bottom";
  system.defaults.dock.show-process-indicators = true;
  system.defaults.dock.show-recents = false;
  system.defaults.dock.showhidden = true;
  system.defaults.dock.static-only = true;
  system.defaults.dock.tilesize = 32;

  system.defaults.spaces.spans-displays = false;

  ##: Corner hot actions
  # 1 => Disabled
  # 2 => Mission Control
  # 3 => Application Windows
  # 4 => Desktop
  # 5 => Start Screen Saver
  # 6 => Disable Screen Saver
  # 7 => Dashboard
  # 10 => Put Display to Sleep
  # 11 => Launchpad
  # 12 => Notification Center
  # 13 => Lock Screen
  # 14 => Quick Note
  system.defaults.dock.wvous-bl-corner = 5;
  system.defaults.dock.wvous-br-corner = 1;
  system.defaults.dock.wvous-tl-corner = 1;
  system.defaults.dock.wvous-tr-corner = 1;

  ###: WINDOW MANAGER / FINDER =================================================

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.AppleShowAllFiles = true;

  # Whether to display icons on the desktop.
  system.defaults.finder.CreateDesktop = false;

  system.defaults.finder.FXDefaultSearchScope = null;
  system.defaults.finder.FXEnableExtensionChangeWarning = false;

  # Finder layout style
  # "icnv" => Icon view
  # "Nlsv" => List view
  # "clmv" => Column View
  # "Flwv" => Gallery View
  system.defaults.finder.FXPreferredViewStyle = "Nlsv";

  system.defaults.finder.QuitMenuItem = false;
  system.defaults.finder.ShowPathbar = true;
  system.defaults.finder.ShowStatusBar = true;
  system.defaults.finder._FXShowPosixPathInTitle = false;

  system.defaults.NSGlobalDomain.NSDocumentSaveNewDocumentsToCloud = false;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;

  # Set the size of the finder sidebar icons
  # 1 => small; 2 => medium; 3 => large
  system.defaults.NSGlobalDomain.NSTableViewDefaultSizeMode = 1;

  system.defaults.NSGlobalDomain.NSTextShowsControlCharacters = true;

  # Disable the over-the-top focus ring animation
  system.defaults.NSGlobalDomain.NSUseAnimatedFocusRing = false;

  system.defaults.NSGlobalDomain.NSWindowResizeTime = 0.001;
  system.defaults.NSGlobalDomain.PMPrintingExpandedStateForPrint = true;
  system.defaults.NSGlobalDomain.PMPrintingExpandedStateForPrint2 = true;
  system.defaults.NSGlobalDomain._HIHideMenuBar = l.mkDefault false;

  # Set the spring loading delay for directories. The default is the float `1.0`.
  system.defaults.NSGlobalDomain."com.apple.springing.delay" = 0.1;

  # Enable spring loading (expose) for directories.
  system.defaults.NSGlobalDomain."com.apple.springing.enabled" = true;

  ###: LOGIN ===================================================================

  system.defaults.loginwindow.DisableConsoleAccess = false;
  system.defaults.loginwindow.GuestEnabled = false;
  # Text to be shown on the login window. Default "\\U03bb".
  # system.defaults.loginwindow.LoginwindowText = null;
  system.defaults.loginwindow.PowerOffDisabledWhileLoggedIn = false;
  system.defaults.loginwindow.RestartDisabled = false;
  system.defaults.loginwindow.RestartDisabledWhileLoggedIn = false;
  system.defaults.loginwindow.SHOWFULLNAME = false;
  system.defaults.loginwindow.ShutDownDisabled = false;
  system.defaults.loginwindow.ShutDownDisabledWhileLoggedIn = false;
  system.defaults.loginwindow.SleepDisabled = true;
  system.defaults.loginwindow.autoLoginUser = null;

  ###: POINTING ================================================================

  # 0 to enable Silent Clicking, 1 to disable. The default is 1.
  system.defaults.trackpad.ActuationStrength = 1;
  # Whether to enable trackpad tap to click. The default is false.
  system.defaults.trackpad.Clicking = true;
  system.defaults.trackpad.Dragging = false;
  # For normal click: 0 for light clicking, 1 for medium, 2 for firm. The default is 1.
  system.defaults.trackpad.FirstClickThreshold = 1;
  # For force touch: 0 for light clicking, 1 for medium, 2 for firm. The default is 1.
  system.defaults.trackpad.SecondClickThreshold = 1;
  system.defaults.trackpad.TrackpadRightClick = true;
  system.defaults.trackpad.TrackpadThreeFingerDrag = true;

  ##: Enables swiping left or right with two fingers to navigate backward or forward.
  system.defaults.NSGlobalDomain.AppleEnableMouseSwipeNavigateWithScrolls = true;
  system.defaults.NSGlobalDomain.AppleEnableSwipeNavigateWithScrolls = true;

  system.defaults.magicmouse.MouseButtonMode = "TwoButton";
  # Show scroll bars when an external mouse or trackball is connected.
  system.defaults.NSGlobalDomain.AppleShowScrollBars = "Automatic";
  system.defaults.NSGlobalDomain.NSScrollAnimationEnabled = true;
  # Configures the trackpad tap behavior. Mode 1 enables tap to click.
  system.defaults.NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
  # Disable "Natural" scrolling direction.
  system.defaults.NSGlobalDomain."com.apple.swipescrolldirection" = false;
  # Whether to enable trackpad secondary click.
  system.defaults.NSGlobalDomain."com.apple.trackpad.enableSecondaryClick" = true;
  # Configures the trackpad tracking speed (0 to 3). The default is 1.0.
  system.defaults.NSGlobalDomain."com.apple.trackpad.scaling" = 1.0;
  # Configures the trackpad corner click behavior. Mode 1 enables right click.
  # https://daiderd.com/nix-darwin/manual/index.html#opt-system.defaults.NSGlobalDomain.com.apple.trackpad.trackpadCornerClickBehavior
  system.defaults.NSGlobalDomain."com.apple.trackpad.trackpadCornerClickBehavior" = null;
  system.defaults.universalaccess.closeViewScrollWheelToggle = true;
  system.defaults.universalaccess.closeViewZoomFollowsFocus = true;

  ###: KEYBOARD ===================================================================

  # Configures the keyboard control behavior. Mode 3 enables full keyboard control.
  system.defaults.NSGlobalDomain.AppleKeyboardUIMode = 3;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 2;
  system.defaults.NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticDashSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticPeriodSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticQuoteSubstitutionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticWindowAnimationsEnabled = false;
  # Whether to use F1, F2, etc. keys as standard function keys.
  system.defaults.NSGlobalDomain."com.apple.keyboard.fnState" = true;

  system.keyboard.enableKeyMapping = true;
  system.keyboard.nonUS.remapTilde = false;
  system.keyboard.remapCapsLockToControl = false;
  system.keyboard.remapCapsLockToEscape = true;

  system.keyboard.swapLeftCommandAndLeftAlt = false;

  ###: PROCESSES / ACTIVITY MONITOR ========================================================

  # Disable automatic termination of "inactive" apps.
  system.defaults.NSGlobalDomain.NSDisableAutomaticTermination = true;

  # 0 => Application Icon
  # 2 => Network Usage
  # 3 => Disk Activity
  # 5 => CPU Usage
  # 6 => CPU History
  system.defaults.ActivityMonitor.IconType = 5;
  system.defaults.ActivityMonitor.OpenMainWindow = true;
  # 100 => All Processes
  # 101 => All Processes, Hierarchally
  # 102 => My Processes
  # 103 => System Processes
  # 104 => Other User Processes
  # 105 => Active Processes
  # 106 => Inactive Processes
  # 107 => Windowed Processes
  system.defaults.ActivityMonitor.ShowCategory = 100;
  system.defaults.ActivityMonitor.SortColumn = "CPUUsage";
  # 0 => descending
  system.defaults.ActivityMonitor.SortDirection = 0;

  ###: MISC ====================================================================

  system.defaults.CustomSystemPreferences = {};
  system.defaults.CustomUserPreferences = {};

  system.defaults.screencapture.disable-shadow = false;
  # The filesystem path to which screencaptures should be written.
  # system.defaults.screencapture.location = null;
  system.defaults.screencapture.type = "png";
}
