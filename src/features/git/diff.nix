{
  dotfield.features.development.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.difftastic # <- syntax-aware structural diff tool
      ];

      programs.git.extraConfig.rerere.enabled = true;
    };
}
