{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs.stdenv.hostPlatform) isAarch64;
in {
  # NOTE: This is NOT the same as upstream's `homebrew.brewPrefix` option,
  # which defaults to the equivalent of `$(brew --prefix)/bin`.
  # <https://github.com/LnL7/nix-darwin/issues/596>
  brewPrefix =
    if isAarch64
    then "/opt/homebrew"
    else "/usr/local";
}
