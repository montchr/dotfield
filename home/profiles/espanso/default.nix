{
  pkgs,
  # config,
  ...
}:
let
  # inherit (config.lib.file) mkOutOfStoreSymlink;
  # dotfieldDir = config.home.sessionVariables."DOTFIELD_DIR";

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
  imports = [
    ./matches/base.nix
    ./matches/time.nix
    ./matches/urls.nix
    ./matches/words.nix
    ./matches/comment.nix
    ./matches/_accented-words.nix
  ];

  services.espanso = {
    enable = true;
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
  };

  home.packages = [ cmx-espanso-list-triggers ];
}
