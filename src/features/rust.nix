{
  dotfield.features.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.rustup
        pkgs.rustic
      ];
    };
}
