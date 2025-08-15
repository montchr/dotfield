flake@{ ... }:
{
  dotfield.nixos = {
    home-manager = {
      backupFileExtension = "bak";
      useGlobalPkgs = true;
      useUserPackages = true;
      verbose = true;
    };
  };

  dotfield.home =
    { config, ... }:
    let
      meta = (flake.config.dotfield.meta.users.${config.home.username});
    in

    {
      _module.args = {
        inherit (meta) whoami;
        prefs = meta.preferences;
      };
      programs.home-manager.enable = true;
      manual.json.enable = true;
    };

  dotfield.features.graphical.home = {
    # TODO: what even is this for?
    systemd.user.targets.tray = {
      Unit = {
        Description = "Home Manager System Tray";
        Requires = [ "graphical-session-pre.target" ];
        # Any service starting after tray.target also needs to start
        # after "graphical-session.target" to prevent cyclic dependency.
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
