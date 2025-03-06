{ pkgs, ... }:
{
  programs.chromium = {
    enable = true;
    package = pkgs.chromium.override { enableWideVine = true; };
    extensions = [
      { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # <- dark reader
      { id = "ldgfbffkinooeloadekpmfoklnobpien"; } # <- raindrop.io
      { id = "fmkadmapgofadopljbjfkapdkoienihi"; } # <- react devtools
    ];
  };
}
