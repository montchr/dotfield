{
  inputs,
  profiles,
  ...
}: {
  imports = [profiles.core.substituters.nixpkgs-wayland];
  nix.settings.trusted-substituters = ["https://nixpkgs-wayland.cachix.org"];
  nixpkgs.overlays = [inputs.nixpkgs-wayland.overlay];
}
