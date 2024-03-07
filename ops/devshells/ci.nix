{
  perSystem =
    { pkgs, ... }:
    {
      devshells.dotfield-ci = {
        devshell.name = "dotfield-ci";
        devshell.packages = [
          pkgs.cachix
          pkgs.deadnix
          pkgs.just
          pkgs.nvd
          pkgs.shellcheck
          pkgs.statix
        ];
      };
    };
}
