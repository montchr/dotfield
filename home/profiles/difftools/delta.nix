{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [delta];
  programs.git.delta = {
    enable = lib.mkDefault (!config.programs.git.difftastic.enable);
    options = {
      line-numbers = true;
      navigate = true;
      keep-plus-minus-markers = true;
    };
  };
}
