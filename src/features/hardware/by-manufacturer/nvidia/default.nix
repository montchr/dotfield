{
  aspects.hardware__nvidia = {
    nixos = {
      programs.sway.extraOptions = [ "--unsupported-gpu" ];
    };

    home = {
      programs.mpv.config.hwdec = "vdpau";
    };
  };
}
