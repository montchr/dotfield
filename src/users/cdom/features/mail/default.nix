{ lib, ... }:
{
  users.cdom.aspects.mail.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.aerc
        pkgs.pizauth
      ];

      programs.mbsync.enable = true;
      programs.msmtp.enable = true;
      programs.notmuch.enable = true;
      services.mbsync = {
        enable = true;
        frequency = "*:0/5";
        # Absolute path to package is necessary.
        postExec = "${lib.getExe pkgs.notmuch} new";
      };

      accounts.email = {
        maildirBasePath = "Mail";
      };
    };
}
