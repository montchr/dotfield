{
  flake,
  pkgs,
  ...
}:
let
  inherit (flake.inputs) haumea;
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;

  cmx-espanso-list-triggers = pkgs.writeShellApplication {
    name = "cmx-espanso-list-triggers";
    runtimeInputs = [ pkgs.ripgrep ];
    # TODO: use a yaml parser (yq or whatever)
    text = ''
      rg '^\s*(-\s)?(trigger:\s)(.+)$'
    '';
  };
in
{
  services.espanso = {
    enable = true;
    package = if isLinux then pkgs.espanso-wayland else pkgs.espanso;
    configs = {
      default = {
        search_trigger = ";;snip";

        # Valid hotkey values listed here:
        # <https://espanso.org/docs/configuration/options/#customizing-the-search-shortcut>
        # <https://github.com/federico-terzi/espanso/blob/283b85818b6cc27f1d545337b99effa847b380eb/espanso-detect/src/hotkey/keys.rs#L221-L302>
        # FIXME: this is mac only -- fix for pc layout
        # search_shortcut = "ALT+CMD+SPACE";
      };
    };
    matches = haumea.lib.load {
      src = ./matches;
      loader = haumea.lib.loaders.verbatim;
    };
  };

  home.packages = [ cmx-espanso-list-triggers ];
}
