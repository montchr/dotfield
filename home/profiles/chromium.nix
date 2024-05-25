{
  programs.chromium = {
    enable = true;
    extensions = [
      { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # <- dark reader
      { id = "ldgfbffkinooeloadekpmfoklnobpien"; } # <- raindrop.io
      { id = "fmkadmapgofadopljbjfkapdkoienihi"; } # <- react devtools
    ];
  };
}
