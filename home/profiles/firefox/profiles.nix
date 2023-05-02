hmArgs @ {
  config,
  inputs,
  pkgs,
  inputs',
  ...
}: let
  inherit (inputs.apparat.lib.firefox) evalSettings;
  inherit (inputs.apparat.lib) rake withHexPrefixes;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config) theme;
  inherit (config.theme) fonts;
  l = inputs.nixpkgs.lib // builtins;

  cfg = config.programs.firefox;
  mixins = rake ./mixins;

  addons = inputs'.firefox-addons.packages;
  extensions = import ./extensions/common.nix {inherit addons;};

  imp = s: s + " !important";

  colors = withHexPrefixes theme.colors.active.colors;
  colorsI = l.mapAttrs (_: imp) colors;

  makeSettings = {modules ? []}:
    evalSettings {
      inherit theme;
      modules = [./settings/common.nix] ++ modules;
      hmConfig = config;
      osConfig = hmArgs.osConfig or null;
    };
  makeSettings' = module: (makeSettings {modules = [module];}).config;

  fontStack = l.concatMapStrings (y: ''"${y}", '');
  fontStack' = x: fontStack (l.map (y: y.family) (l.toList x));

  ffSans = fontStack' fonts.sans + "sans-serif";
  ffMono = fontStack' fonts.mono + "monospace";
  ffTerm = (fontStack' [fonts.term fonts.mono]) + "monospace";

  userChrome = with mixins.userChrome; ''
    * {
      font-family: ${imp ffTerm};
    }

    /*
    FIXME: auto-hiding tabs is difficult to use...
    ${import autoHideTabs}
    */

    #sidebar-header {
      display: none;
    }
  '';
  userContent = with mixins.userContent; ''
    ${import monospaceText ''
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

  search = {
    default = "Kagi";
    engines = {
      "Kagi" = {urls = [{template = "https://kagi.com/search?q={searchTerms}";}];};
      "Bundlephobia" = {
        urls = [{template = "https://bundlephobia.com/packages/{searchTerms}";}];
        definedAliases = ["@bp"];
      };
      "npm" = {
        urls = [{template = "https://www.npmjs.com/search?q={searchTerms}";}];
        definedAliases = ["@npm"];
      };
      "Nix Packages" = import ./search/nix-packages.nix {inherit pkgs;};
      "NixOS Wiki" = import ./search/nixos-wiki.nix;
    };
    force = true;
  };
in {
  imports = [./extensions/tridactyl];

  programs.firefox.profiles.home = {
    inherit userChrome userContent search;
    id = 0;
    extensions = extensions ++ [addons.tridactyl];
    settings = makeSettings' {
      imports = [./settings/browser-toolbox.nix];
      "browser.startup.homepage" = "https://lobste.rs";
    };
  };

  programs.firefox.profiles.work = {
    inherit userContent;
    id = 1;
    extensions = extensions ++ [addons.tridactyl];
    # search = dmerge.merge search {
    #   engines.lucideIcons = import ./search/lucide-icons.nix;
    # };
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
