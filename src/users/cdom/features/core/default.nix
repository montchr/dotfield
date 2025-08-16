{
  dotfield.baseline.home =
    { pkgs, ... }:
    {
      home.shellAliases."grr" = "${pkgs.bat-extras.batgrep}/bin/batgrep";
      home.shellAliases."man" = "${pkgs.bat-extras.batman}/bin/batman";
    };
}
