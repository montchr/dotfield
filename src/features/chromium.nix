{
  aspects.graphical.home =
    { pkgs, ... }:
    {
      programs.chromium = {
        enable = true;
        package = pkgs.chromium.override { enableWideVine = true; };
      };
    };
}
