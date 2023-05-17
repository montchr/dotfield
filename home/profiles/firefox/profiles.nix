hmArgs @ {
  config,
  inputs,
  pkgs,
  inputs',
  ...
}: let
  inherit (inputs) apparat haumea;
  inherit (apparat.lib.color) withHexPrefixes;
  inherit (apparat.lib.firefox) evalSettings;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config) theme;
  inherit (config.theme) fonts;
  l = inputs.nixpkgs.lib // builtins;

  cfg = config.programs.firefox;
  mixins = haumea.lib.load {
    src = ./mixins;
    inputs = {inherit l;};
  };

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

  userChrome = let
    inherit (mixins.common) themeSettings;
  in ''
    @charset "UTF-8";
    @namespace xul "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul";
    @namespace html "http://www.w3.org/1999/xhtml";

    :root {
      ${themeSettings {inherit colors fonts;}}
    }

    * {
      font-family: var(--dotfield--font--mono, monospace) !important;
      font-size: 10px;
      line-height: 1;
    }

    moz-input-box,
    #urlbar-input-container {
      font-size: 12px;
    }

    #sidebar-header {
      display: none;
    }
  '';

  # FIXME: userContent.css does not appear to be loading in browser
  userContent = let
    inherit (mixins.common) themeSettings;
    inherit (mixins.userContent) monospaceText;
  in ''
    @charset "UTF-8";
    @namespace xul "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul";
    @namespace html "http://www.w3.org/1999/xhtml";

    :host,
    :root {
      ${themeSettings {inherit colors fonts;}}
    }

    ${monospaceText ''
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
  imports = [./extensions/tridactyl.nix];

  programs.firefox.profiles.home = {
    inherit extensions userChrome userContent search;
    id = 0;
    settings = makeSettings' {
      imports = [./settings/browser-toolbox.nix];
      "browser.startup.homepage" = "https://lobste.rs";
    };
  };

  programs.firefox.profiles.work = {
    inherit extensions userContent;
    id = 1;
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
