{
  users.cdom.aspects.desktop-sessions__wayland-wm.home =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      swaylock = lib.getExe config.programs.swaylock.package;
      pgrep = "${pkgs.procps}/bin/pgrep";
      lockTime = 10 * 60;
    in
    {
      services.swayidle = {
        enable = true;
        # FIXME: this isn't quite right, but the daemon needs to be
        # activated after the session is ready.  the defaults aren't
        # cutting it.
        systemdTarget = "tray.target";
        timeouts = [
          {
            timeout = lockTime;
            command = "${swaylock} --daemonize";
          }
        ];
      };
    };
}
