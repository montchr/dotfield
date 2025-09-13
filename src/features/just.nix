{
  aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.just
        pkgs.just-lsp
      ];
    };
}
