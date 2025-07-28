{
  # TODO: <https://gitlab.com/wef/dotfiles/-/blob/master/bin/mylock>
  dotfield.modules."desktop-environments/wayland-wm".home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.wlr-which-key
      ];
    };
}
