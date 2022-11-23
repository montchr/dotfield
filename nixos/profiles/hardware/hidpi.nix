_: {
  hardware.video.hidpi.enable = true;
  fonts.fontconfig = {
    # NOTE: NixOS docs suggest the following settings may be preferable on hidpi
    # displays, though that has not yet been the case in my experience. I have
    # still needed to enable antialiasing on HodgePodge, and the other settings
    # don't appear to have an effect with antialiasing enabled. Until I
    # encounter a situation where these settings actually seem sensible, they
    # remain at their default values.
    #
    # antialias = mkDefault false;
    # subpixel.lcdfilter = mkDefault "none";
    # hinting.enable = false;
  };
}
