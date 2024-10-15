{ lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

in
{
  imports = [
    ./google.nix
    ./protonmail.nix
  ];

  programs.mbsync.enable = true;
  programs.mu.enable = true;
  programs.msmtp.enable = true;
  services.mbsync = {
    enable = false;
    frequency = "*:0/5";
    # TODO: might need to be told about password store dir
    postExec = "${pkgs.mu}/bin/mu index";
  };

  accounts.email = {
    maildirBasePath = "Mail";

  };
}
## References:
# https://github.com/Emiller88/dotfiles/blob/5eaabedf1b141c80a8d32e1b496055231476f65e/modules/shell/mail.nix
