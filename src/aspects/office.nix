{
  dotfield.aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.libreoffice-fresh
        pkgs.kdePackages.okular
        pkgs.xournalpp # pdf annotation
      ];
    };
}
