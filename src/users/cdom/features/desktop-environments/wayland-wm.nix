{
  # TODO: <https://gitlab.com/wef/dotfiles/-/blob/master/bin/mylock>
  dotfield.features.wayland-wm.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.wlr-which-key
      ];
    };
}
