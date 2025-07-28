{
  dotfield.modules.workstation.home =
    { config, pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;

      espanso-list-triggers = pkgs.writeShellApplication {
        name = "espanso-list-triggers";
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

            # <https://espanso.org/docs/configuration/options/#customizing-the-search-shortcut>
            # TODO: verify
            search_shortcut = if isDarwin then "CMD+ALT+SPACE" else "ALT+SHIFT+SPACE";
          };
        };
      };

      home.packages = [ espanso-list-triggers ];
    };
}
