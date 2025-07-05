{ config, ... }:
let
  inherit (config.dotfield) whoami;
in
{
  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        inherit (whoami) email name;
      };
    };
  };
}
