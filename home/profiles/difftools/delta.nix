{ config, ... }:
let
  themePolarity = config.theme.color.schemes.default.kind;
in
{
  programs.git.delta = {
    enable = true;
    options = {
      light = themePolarity == "light";
      line-numbers = true;
      navigate = true;
      keep-plus-minus-markers = true;
    };
  };
}
