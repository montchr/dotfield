{
  # TODO: move to home
  flake.modules.nixos.workstation =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.signal-desktop
      ];
    };
}
