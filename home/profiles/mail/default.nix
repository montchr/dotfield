{ pkgs, ... }:
{
  imports = [
    ./accounts
  ];

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
    postExec = "notmuch new";
  };

  accounts.email = {
    maildirBasePath = "Mail";
  };
}
