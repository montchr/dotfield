{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        ffmpeg
        sox
        tenacity
      ];
    };
}
