# TODO: do theme stuff in theme profile / only if theme enabled
hmArgs @ {
  config,
  pkgs,
  flake,
  ...
}: let
  inherit (flake.inputs) apparat haumea;
  inherit (flake.perSystem.inputs') firefox-addons;
  inherit (flake.perSystem.packages) firefox-ui-fix;
  inherit (apparat.lib.firefox) evalSettings paths;
  inherit (pkgs.stdenv) hostPlatform;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config) theme;
  l = flake.inputs.nixpkgs.lib // builtins;

  cfg = config.programs.firefox;

  profilesPath = (paths hostPlatform).profiles;

  mixins = haumea.lib.load {
    src = ./mixins;
    inputs = {inherit l;};
  };

  addons = firefox-addons.packages;
  extensions = import ./extensions/common.nix {inherit addons;};

  makeSettings = {modules ? []}:
    evalSettings {
      inherit theme;
      modules = [./settings/common.nix] ++ modules;
      extraArgs = {
        hmConfig = config;
        osConfig = hmArgs.osConfig or null;
      };
    };
  makeSettings' = module: (makeSettings {modules = [module];}).config;

  userChrome = let
    inherit (mixins.common) themeSettings;
  in ''
    @import url("css/leptonChrome.css");

    :root {
      ${themeSettings {inherit (theme) fonts;}}
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

  # FIXME: userContent.css is only for mozilla pages! e.g. about:config
  #        most of this should be moved to something like Stylus
  userContent = let
    inherit (mixins.common) themeSettings;
    inherit (mixins.userContent) monospaceText;
  in ''
    @import url("css/leptonContent.css");

    :host,
    :root {
      ${themeSettings {inherit (theme) fonts;}}
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
      imports = [
        ./settings/browser-toolbox.nix
        ./settings/lepton.nix
      ];
      "browser.startup.homepage" = "https://lobste.rs";
    };
  };

  programs.firefox.profiles.work = {
    inherit extensions userContent;
    userChrome = ''
      @import url("css/leptonChrome.css");
    '';
    id = 1;

    # search = dmerge.merge search {
    #   engines.lucideIcons = import ./search/lucide-icons.nix;
    # };

    # FIXME: distinguish appearance from other profile
    settings = makeSettings' {
      imports = [./settings/lepton.nix];
      "browser.startup.homepage" = "about:blank";
      "userChrome.theme.monospace" = false;
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

  # TODO: extract to function
  home.file = l.mkMerge (l.flip l.mapAttrsToList cfg.profiles (_: profile: let
    profileDir = "${profilesPath}/${profile.path}";
  in {
    "${profileDir}/chrome/css".source = "${firefox-ui-fix}/chrome/css";
    "${profileDir}/chrome/icons".source = "${firefox-ui-fix}/chrome/icons";
  }));
}
