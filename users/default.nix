{ config, lib, pkgs, ... }:
{
  imports = [
    ./primary
    ./profiles/mail
    ./profiles/pass
  ];
}
