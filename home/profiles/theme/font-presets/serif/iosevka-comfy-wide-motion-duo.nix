{ pkgs, ... }:
{
  theme.font.serif = {
    name = "Iosevka Comfy Wide Motion Duo";
    package = pkgs.iosevka-comfy.comfy-wide-motion-duo;
    # TODO: might be wrong
    psNamespace = "Iosevka-Comfy-Wide-Motion-Duo";
  };
}
