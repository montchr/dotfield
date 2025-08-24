{
  dotfield.aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.rustup
        pkgs.rustic
      ];
    };
}
