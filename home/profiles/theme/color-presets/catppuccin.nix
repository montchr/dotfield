{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (flake.inputs.base16-schemes.lib) schemes;
  inherit (flake.self.lib.theme) mkColorScheme;

  cfg = config.theme;

  # <https://github.com/catppuccin/nix/blob/b1ff2a638afa827f1473498190a2c1cae1cf41cf/modules/home-manager/chrome.nix#L6-L11>
  chromeExtensionIds = {
    "Catppuccin Frappe" = "olhelnoplefjdmncknfphenjclimckaf";
    "Catppuccin Latte" = "jhjnalhegpceacdhbplhnakmkdliaddd";
    "Catppuccin Macchiato" = "cmpdlhmnmjhihmcfnigoememnffkimlk";
    "Catppuccin Mocha" = "bkkmolkhemgaeaeggcmfbghljjjoofoh";
  };
  supportedChromiumBrowsers = [ "chromium" ];
in
lib.mkIf cfg.enable (
  lib.mkMerge [
    {
      theme.color.scheme.dark = mkColorScheme schemes.catppuccin-mocha;
      theme.color.scheme.light = mkColorScheme schemes.catppuccin-latte;
    }
  ]
  ++ (builtins.map (browser: {
    programs.${browser}.extensions = [
      { id = chromeExtensionIds.${cfg.color.scheme.dark.name}; }
      { id = chromeExtensionIds.${cfg.color.scheme.light.name}; }
    ];
  }) supportedChromiumBrowsers)
)
