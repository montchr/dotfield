moduleArgs @ {lib, ...}:
lib.mkIf (!(moduleArgs.osConfig.home-manager.useGlobalPkgs or false)) {
  # https://github.com/nix-community/home-manager/issues/2942
  nixpkgs.config.allowUnfreePredicate = _pkg: true;
}
