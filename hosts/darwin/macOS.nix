{ config, lib, pkgs, ... }:

{
  users.users.admin = {
    extraGroups = ["admin" "wheel"];
  };
}
