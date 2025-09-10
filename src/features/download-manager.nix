{
  aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.kdePackages.kget
      ];
    };
}
