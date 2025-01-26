{
  config,
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
  imports = [
    ./matches/arrows.nix
    ./matches/base.nix
    ./matches/comment.nix
    ./matches/crypt.nix
    ./matches/identity.nix
    ./matches/punctuation.nix
    ./matches/time.nix
    ./matches/urls.nix
    ./matches/words.nix
    ./matches/_accented-words.nix
  ];
  services.espanso = {
    enable = true;
    package = if isLinux then pkgs.espanso-wayland else pkgs.espanso;
    configs = {
      default = {
        search_trigger = ";;snip";

        # <https://espanso.org/docs/configuration/options/#customizing-the-search-shortcut>
        # TODO: verify
        search_shortcut = if isDarwin then "CMD+ALT+SPACE" else "ALT+SHIFT+SPACE";
      };
    };
  };

  home.packages = [ cmx-espanso-list-triggers ];
}
