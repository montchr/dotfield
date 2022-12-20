{
  inputs,
  self,
  ...
}: {
  imports = [(self + "/profiles/core/substituters/nixpkgs-wayland.nix")];
  nix.settings.trusted-substituters = ["https://nixpkgs-wayland.cachix.org"];
  nixpkgs.overlays = [inputs.nixpkgs-wayland.overlay];
}
