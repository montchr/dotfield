{
  perSystem =
    { pkgs, ... }:
    {
      devshells.dotfield-ci = {
        devshell.name = "dotfield-ci";
        devshell.packages = (import ./__common-packages.nix pkgs) ++ [ ];
      };
    };
}
