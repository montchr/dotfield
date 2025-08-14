{ withSystem, ... }:
{
  dotfield.features."fonts/berkeley-mono".nixos =
    { pkgs, ... }:
    withSystem pkgs.stdenv.hostPlatform.system (
      { packages, inputs', ... }:
      {
        fonts.packages = [ packages.berkeley-mono ];
      }
    );
}
