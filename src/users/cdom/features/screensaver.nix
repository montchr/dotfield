{ lib, ... }:
{
  users.cdom.aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.xscreensaver ];

      # services.swayidle.timeouts = [
      #   {
      #     # timeout = 60;
      #     timeout = 10;
      #     command = lib.getExe pkgs.xscreensaver;
      #     resumeCommand = "killall ${lib.getExe pkgs.xscreensaver}";
      #   }
      # ];
    };
}
