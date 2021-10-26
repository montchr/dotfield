{ config, lib, pkgs, ... }:
{
  imports = [
    ./primary
    ./profiles/fish
    ./profiles/mail
    ./profiles/pass
  ];
}
