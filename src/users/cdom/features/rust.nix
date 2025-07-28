{
  dotfield.modules.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.rustlings
      ];
    };
}
