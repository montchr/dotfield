{ config, lib, pkgs, ... }:

let
  inherit (config.dotfield) profilesDir;

  mailDir = "${config.my.user.home}/Mail";

  gmailAccount =
    { name
    , domain
    , realName ? config.my.name
    , username ? "chris"
    , ...
    }:
    let address = "${username}@${domain}"; in
    {
      inherit address realName;

      userName = address;
      flavor = "gmail.com";
      passwordCommand = "${pkgs.pass}/bin/pass Email/${domain}/${username}--mbsync";
      maildir.path = name;

      mu.enable = true;

      mbsync = {
        enable = true;
        create = "maildir";
        remove = "none";
        expunge = "both";
        groups.${name}.channels = lib.mapAttrs
          (_: v: v // {
            extraConfig = {
              Create = "Near";
              CopyArrivalDate = "yes";
              MaxMessages = 1000000;
              MaxSize = "10m";
              Sync = "All";
              SyncState = "*";
            };
          })
          {
            inbox = {
              farPattern = "";
              nearPattern = "inbox";
              extraConfig.Expunge = "Both";
            };
            trash = {
              farPattern = "[Gmail]/Trash";
              nearPattern = "trash";
            };
            sent = {
              farPattern = "[Gmail]/Sent Mail";
              nearPattern = "sent";
              extraConfig.Expunge = "Both";
            };
            archive = {
              farPattern = "[Gmail]/All Mail";
              nearPattern = "archive";
            };
            starred = {
              farPattern = "[Gmail]/Starred";
              nearPattern = "starred";
            };
            drafts = {
              farPattern = "[Gmail]/Drafts";
              nearPattern = "drafts";
              extraConfig.Expunge = "Both";
            };
          };
      };

      # https://tecosaur.github.io/emacs-config/config.html#fetching
      # TODO: for faster sync. needs configuration.
      # imapnotify = {
      #   enable = false;
      #   boxes = [ "Inbox" ];
      # };
    };
in
{
  # FIXME: infinite recursion!
  # imports = [ "${profilesDir}/mail" ];

  my.hm = {
    programs.mbsync.enable = true;
    programs.mu.enable = true;

    accounts.email = {
      maildirBasePath = mailDir;
      accounts = {
        personal = gmailAccount
          {
            name = "personal";
            domain = "cdom.io";
          } // {
          msmtp.enable = true;
        };
        work = gmailAccount {
          name = "work";
          domain = "alley.co";
        };
      };
    };
  };
}

## References:
# https://github.com/Emiller88/dotfiles/blob/5eaabedf1b141c80a8d32e1b496055231476f65e/modules/shell/mail.nix
# https://github.com/berbiche/dotfiles/blob/cf8bc65bb775b69727a660a75ef2b981b0a31e54/profiles/email/accounts.nix
