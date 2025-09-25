{
  aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        vhs
      ];
    };
}
