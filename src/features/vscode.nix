{
  dotfield.aspects.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.vscode
      ];
    };
}
