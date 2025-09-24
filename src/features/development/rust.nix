{
  aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.rustc
        pkgs.rustfmt
        pkgs.rustlings
      ];
    };
}
