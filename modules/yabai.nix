{ config, lib, pkgs, ... }:

{
  options.services.yabai.border.enable = lib.mkEnableOption ''
    Whether to enable border functionality for yabai.

    Border functionality is noted to perform badly on macOS 12 (Monterey).
  '';
}
