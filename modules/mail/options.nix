{ options, lib, ... }:
{
  options.my.modules.mail.enable = lib.mkEnableOption false;
}
