    {inputs',
    pkgs,
    lib,
    ...
  }: _devshellArgs: let
    inherit (inputs'.deadnix.packages) deadnix;
  in {
      name = "dotfield-ci";
      packages = with pkgs; [
        cachix
        deadnix
        just
        nvd
        nvfetcher
        shellcheck
        statix
      ];
}
