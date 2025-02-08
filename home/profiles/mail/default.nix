{ pkgs, ... }:
{
  imports = [ ./accounts/default.nix ];

  home.packages = [
    pkgs.aerc
    pkgs.isync
    pkgs.mu
    pkgs.mutt
    pkgs.notmuch
    pkgs.notmuch-mutt
    pkgs.pizauth
  ];

  programs.mbsync.enable = true;
  programs.mu.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch.enable = true;
  services.mbsync = {
    enable = false;
    frequency = "*:0/5";
    # TODO: might need to be told about password store dir
    postExec = "${pkgs.mu}/bin/mu index";
  };

  accounts.email = {
    maildirBasePath = "Mail";
  };

  # programs.emacs.extraPackages = _epkgs: [ pkgs.mu  ];
}
## References:
# https://github.com/Emiller88/dotfiles/blob/5eaabedf1b141c80a8d32e1b496055231476f65e/modules/shell/mail.nix
