{ flake, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = flake.inputs.nixpkgs.lib // builtins;
in
l.mkIf (!isDarwin) {
  programs.chromium = {
    enable = true;
    extensions = [
      { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # <- dark reader
      { id = "ldgfbffkinooeloadekpmfoklnobpien"; } # <- raindrop.io
      { id = "fmkadmapgofadopljbjfkapdkoienihi"; } # <- react devtools
    ];
  };
}
