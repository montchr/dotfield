{
  dotfield.aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.vscode
      ];
    };
}
