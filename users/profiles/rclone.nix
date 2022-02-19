{ config, lib, pkgs, ... }:

{
  my.user.packages = with pkgs; [ rclone ];
}
