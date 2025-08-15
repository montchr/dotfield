{
  dotfield.features.wayland-wm.home =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      swaylock = lib.getExe config.programs.swaylock.package;
      pgrep = "${pkgs.procps}/bin/pgrep";

      isLocked = "${pgrep} -x ${swaylock}";
      lockTime = 10 * 60;

      # Makes two timeouts: one for when the screen is not locked (lockTime+timeout) and one for when it is.
      afterLockTimeout =
        {
          timeout,
          command,
          resumeCommand ? null,
        }:
        [
          {
            timeout = lockTime + timeout;
            inherit command resumeCommand;
          }
          {
            command = "${isLocked} && ${command}";
            inherit resumeCommand timeout;
          }
        ];
    in
    {
      services.swayidle = {
        enable = true;
        timeouts = [
          # Lock screen
          {
            timeout = lockTime;
            command = "${swaylock} --daemonize --grace 15 --grace-no-mouse";
          }
        ]
        # Sway: Turn off displays
        ++ (lib.optionals config.wayland.windowManager.sway.enable (afterLockTimeout {
          timeout = 40;
          command = "swaymsg 'output * dpms off'";
          resumeCommand = "swaymsg 'output * dpms on'";
        }));
      };
    };
}
