{ config, ... }:
let
  inherit (config.dotfield) whoami;
in
{
  programs.jujutsu = {
    enable = true;
    settings = {
      inherit (whoami) email;
      name = whoami.fullName;
    };
  };
}
