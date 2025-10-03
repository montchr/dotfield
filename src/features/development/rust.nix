{
  aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.bacon
        pkgs.rustc
        pkgs.rustfmt
        pkgs.rustlings
      ];
    };
}
