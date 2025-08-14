{
  dotfield.features.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.rustlings
      ];
    };
}
