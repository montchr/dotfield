{
  flake.modules.nixos.graphical =
    { pkgs, ... }:
    {
      programs.firefox.nativeMessagingHosts.packages = [ pkgs.passff-host ];
    };
}
