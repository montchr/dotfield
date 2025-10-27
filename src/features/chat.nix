{
  aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.discord
        pkgs.fluffychat
        pkgs.slack
        pkgs.teams-for-linux
        pkgs.weechat
      ];
    };
}
