{ config, lib, pkgs, suites, profiles, hmUsers, ... }:

let
  extraProfiles = with profiles; [
    users.xtallos
    virtualisation.guests.parallels
  ];
in

{
  imports = with suites;
    base
    ++ extraProfiles;

  home-manager.users.xtallos = {suites, ...}: {
    imports = [hmUsers.xtallos] ++ suites.gui;
  };
}
