{
  # TODO: <https://gitlab.com/wef/dotfiles/-/blob/master/bin/mylock>
  dotfield.users.cdom.aspects.wayland-wm.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.wlr-which-key
      ];
    };
}
