{
  dotfield.aspects.development.home =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        ffmpeg
        ttyd
        vhs
      ];
    };
}
