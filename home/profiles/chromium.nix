{
  inputs,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = inputs.nixpkgs.lib // builtins;
in {
  programs.chromium = {
    enable = true;
    package = l.mkIf isDarwin (pkgs.runCommand "ungoogled-chromium" {} "mkdir $out");
    extensions = [
      {id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";} # <- dark reader
      {id = "ldgfbffkinooeloadekpmfoklnobpien";} # <- raindrop.io
      {id = "fmkadmapgofadopljbjfkapdkoienihi";} # <- react devtools
    ];
  };
}
