{ withSystem, ... }:
{
  dotfield.features.fonts__berkeley-mono.nixos =
    { pkgs, ... }:
    withSystem pkgs.stdenv.hostPlatform.system (
      { packages, inputs', ... }:
      {
        fonts.packages = [ packages.berkeley-mono ];
      }
    );
}
