{
  aspects.noctalia = {
    nixos =
      { pkgs, ... }:
      let
        pluginDeps = [
          pkgs.gpu-screen-recorder
          pkgs.gpu-screen-recorder-gtk
        ];
      in
      {
        environment.systemPackages = pluginDeps ++ [
          pkgs.noctalia
        ];
      };

    home =
      {
        pkgs,
        config,
        lib,
        ...
      }:
      let
        inherit (lib) mkDefault;
        inherit (config.xdg.userDirs) pictures videos;
      in
      {
        programs.noctalia.enable = true;
        programs.noctalia.systemd.enable = true;
        programs.noctalia.settings = {
          brightness.enable_ddcutil = true;
          shell.launch_apps_as_systemd_services = true;
          shell.screenshot.directory = mkDefault "${pictures}/Screenshots";
        };

        home.packages = [ pkgs.ddcutil ];
      };
  };

  aspects.desktop-sessions__niri.home =
    { lib, ... }:
    let
      inherit (lib) mkDefault;
    in
    {
      programs.noctalia.settings = {
        widget.workspaces.label_source = mkDefault "name";
      };
    };
}
