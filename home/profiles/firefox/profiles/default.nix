hmArgs @ {
  config,
  inputs,
  pkgs,
  packages,
  self,
  ...
}: let
  inherit (self.lib.colors) withHexPrefixes;
  inherit (self.lib.apps.firefox) evalSettings mixins;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config) theme;
  inherit (config.theme) fonts;
  l = inputs.nixpkgs.lib // builtins;

  cfg = config.programs.firefox;

  imp = s: s + " !important";

  colors = withHexPrefixes theme.colors.active.colors;
  colorsI = l.mapAttrs (_: imp) colors;

  makeSettings = {modules ? []}:
    evalSettings {
      inherit theme;
      modules = [./settings/common.nix] ++ modules;
      osConfig = hmArgs.osConfig or null;
    };
  makeSettings' = module: (makeSettings {modules = [module];}).config;

  fontStack = l.concatMapStrings (y: ''"${y}", '');
  fontStack' = x: fontStack (l.map (y: y.family) (l.toList x));

  ffSans = fontStack' fonts.sans + "sans-serif";
  ffMono = fontStack' fonts.mono + "monospace";
  ffTerm = (fontStack' [fonts.term fonts.mono]) + "monospace";

  userChrome = ''
    * {
      font-family: ${imp ffTerm};
    }

    #TabsToolbar {
      visibility: collapse;
    }

    #titlebar {
      display: none;
    }

    #sidebar-header {
      display: none;
    }
  '';
  userContent = ''
    ${mixins.monospaceText ''
      font-family: ${imp ffMono};
      font-size: 1em !important;
      line-height: 1.2 !important;
    ''}

    /* GitHub: supplemental text */
    .text-mono {
      font-family: ${imp ffMono};
    }

    body,
    .tooltipped:after,
    .markdown-body,
    .hx_text-body {
      /* Very politely encourage use of our preferred body font family. */
      font-family: ${ffSans};
    }

    @-moz-document url(about:home), url(about:newtab) {
      body {
        --newtab-background-color: ${colorsI.base00};
        --newtab-element-hover-color: ${colorsI.base01};
        --newtab-icon-primary-color: ${colorsI.base04};
        --newtab-search-border-color: ${colorsI.base01};
        --newtab-search-dropdown-color: ${colorsI.base00};
        --newtab-search-dropdown-header-color: ${colorsI.base00};
        --newtab-search-icon-color: ${colorsI.base04};
        --newtab-section-header-text-color: ${colorsI.base05};
        --newtab-snippets-background-color: ${colorsI.base01};
        --newtab-text-primary-color: ${colorsI.base05};
        --newtab-textbox-background-color: ${colorsI.base01};
        --newtab-textbox-border: ${colorsI.base01};
        --newtab-topsites-background-color: ${colorsI.base04};
        --newtab-topsites-label-color: ${colorsI.base05};
      }
    }
  '';
in {
  programs.firefox.profiles.home = {
    inherit userChrome userContent;
    id = 0;
    settings = makeSettings' {
      imports = [./settings/dev.nix];
      "browser.startup.homepage" = "https://lobste.rs";
    };
  };

  programs.firefox.profiles.work = {
    inherit userContent;
    id = 1;
    settings = makeSettings' {
      "browser.startup.homepage" = "about:blank";
    };
  };

  home.packages = l.optional isLinux (pkgs.makeDesktopItem {
    name = "firefox-work-profile";
    desktopName = "Firefox (Work)";
    genericName = "Open a Firefox window scoped to the Work profile.";
    icon = "firefox";
    exec = "${cfg.package}/bin/firefox -P ${cfg.profiles.work.path}";
    categories = ["Application" "Network" "WebBrowser"];
  });

  # home.file = l.mkMerge (l.flip l.mapAttrsToList cfg.profiles (_: profile: let
  #   profileDir = "${profilesPath}/${profile.path}";
  # in {
  #   "${profileDir}/chrome/content-overrides.css".source = ./userContent.css;
  # }));
}
