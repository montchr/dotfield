{ pkgs, ... }:
{
  theme.font.sansSerif = {
    name = "Iosevka Comfy Wide Duo";
    package = pkgs.iosevka-comfy.comfy-wide-duo;
    # TODO: might be wrong
    psNamespace = "Iosevka-Comfy-Wide-Duo";
  };
}
