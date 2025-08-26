{
  dotfield.aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.nixpkgs-review
        pkgs.nix-init
        pkgs.nix-update
      ];
    };
}
