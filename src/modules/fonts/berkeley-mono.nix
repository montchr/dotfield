{ withSystem, ... }:
{
  dotfield.modules.graphical.nixos =
    { pkgs, ... }:
    withSystem pkgs.stdenv.hostPlatform.system (
      { packages, inputs', ... }:
      {
        fonts.packages = [ packages.berkeley-mono ];
      }
    );
}
