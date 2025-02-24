# TODO: do theme stuff in theme profile / only if theme enabled
hmArgs@{
  config,
  pkgs,
  flake,
  lib,
  ...
}:
let
  inherit (flake.inputs) haumea;
  inherit (flake.perSystem.inputs') firefox-addons;
  inherit (flake.inputs.apparat.lib.firefox) evalSettings;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config) theme;
  lib' = flake.self.lib;

  cfg = config.programs.firefox;

  mixins = haumea.lib.load {
    src = ./mixins;
    inputs = {
      inherit lib;
    };
  };

  addons = firefox-addons.packages;
  extensions = import ./extensions/common.nix { inherit addons; };

  makeSettings =
    {
      modules ? [ ],
    }:
    evalSettings {
      inherit theme;
      modules = [
        ./settings/common.nix
        {
          "identity.fxaccounts.account.device.name" =
            hmArgs.osConfig.networking.hostName or (builtins.getEnv "HOSTNAME");
        }
        { _module.args = { inherit lib'; }; }
      ] ++ modules;
      extraArgs = {
        inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
      };
    };
  makeSettings' = module: (makeSettings { modules = [ module ]; }).config;

  userChrome = ''
    :root {
      ${mixins.common.themeSettings { inherit (theme) fonts; }}
    }

    * {
      font-family: var(--dotfield--font--mono, monospace) !important;
      font-size: 11px;
      line-height: 1;
    }

    moz-input-box,
    #urlbar-input-container {
      font-size: 13px;
    }

    #sidebar-header {
      /* display: none; */
    }
  '';

  # FIXME: userContent.css is only for mozilla pages! e.g. about:config
  #        most of this should be moved to something like Stylus
  userContent = ''
    :host,
    :root {
      ${mixins.common.themeSettings { inherit (theme) fonts; }}
    }

    ${mixins.userContent.monospaceText ''
      font-family: var(--dotfield--font--mono, monospace) !important;
      font-size: 0.875em !important;
      line-height: 1.1 !important;
    ''}

    /* GitHub: supplemental text */
    .text-mono {
      font-family: var(--dotfield--font--mono, monospace) !important;
    }

    body,
    .tooltipped:after,
    .markdown-body,
    .hx_text-body {
      /* Very politely encourage use of our preferred body font family. */
      font-family: var(--dotfield--font--sans, sans-serif);
    }
  '';

  search = import ./search/default.nix { inherit lib lib' pkgs; };
in
{
  programs.firefox.profiles.home = {
    inherit
      search
      userChrome
      userContent
      ;
    id = 0;
    extensions.packages = extensions;
    settings = makeSettings' {
      imports = [
        ./settings/browser-toolbox.nix

        # TODO: dogfood for a while
        ./settings/ui-state.nix
      ];
      "browser.startup.homepage" = builtins.concatStringsSep "|" [ "https://lobste.rs" ];
    };
  };

  programs.firefox.profiles.work = {
    inherit search userContent;
    id = 1;
    extensions.packages = extensions;
    settings = makeSettings' {
      "browser.startup.homepage" = "about:blank";
      "userChrome.theme.monospace" = false;
    };
  };

  # FIXME: the generic Firefox desktop item does not open the `home` profile by
  # default, hence the definition of the additional `firefox-home-profile`
  home.packages = lib.optionals isLinux [
    # TODO: make this an optional package for per-machine usage
    (pkgs.makeDesktopItem {
      name = "firefox-work-profile";
      desktopName = "Firefox (Work)";
      genericName = "Open a Firefox window scoped to the Work profile.";
      icon = "firefox";
      exec = "${cfg.package}/bin/firefox -P ${cfg.profiles.work.path}";
      categories = [
        "Application"
        "Network"
        "WebBrowser"
      ];
    })
    (pkgs.makeDesktopItem {
      name = "firefox-home-profile";
      desktopName = "Firefox (Home)";
      genericName = "Open a Firefox window scoped to the Home profile.";
      icon = "firefox";
      exec = "${cfg.package}/bin/firefox -P ${cfg.profiles.home.path}";
      categories = [
        "Application"
        "Network"
        "WebBrowser"
      ];
    })
  ];

  # TODO: extract to function
  # home.file = l.mkMerge (l.flip l.mapAttrsToList cfg.profiles (_: profile: let
  #   profileDir = "${profilesPath}/${profile.path}";
  # in {
  # }));
}
