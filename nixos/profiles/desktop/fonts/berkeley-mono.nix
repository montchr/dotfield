{ flake, ... }:
let
  inherit (flake.packages) berkeley-mono;
in
{
  fonts.packages = [ berkeley-mono ];
}
