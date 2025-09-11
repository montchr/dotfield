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
        frequency = "*:0/15";
        # FIXME: no effect -- maybe it needs to use pkgs after all
        postExec = "notmuch new";
      };

      accounts.email = {
        maildirBasePath = "Mail";
      };
    };
}
