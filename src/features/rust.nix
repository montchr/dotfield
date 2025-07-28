{
  dotfield.modules.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.rustup
        pkgs.rustic
      ];
    };
}
