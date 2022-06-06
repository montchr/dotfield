{
  config,
  lib,
  pkgs,
  ...
}: let
  gmailAccount = {
    name,
    domain,
    realName ? "chris montgomery",
    username ? "chris",
    ...
  }: let
    address = "${username}@${domain}";
  in {
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
      groups.${name}.channels =
        lib.mapAttrs
        (_: v:
          v
          // {
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
    # FIXME: needs configuration
    imapnotify = {
      enable = false;
      boxes = [ "Inbox" ];
    };
  };
in {
  home.packages = with pkgs; [
    isync
    mu
  ];

  programs.mbsync.enable = true;
  programs.mu.enable = true;
  programs.msmtp.enable = true;
  services.mbsync = {
    enable = true;
    frequency = "*:0/5";
    # TODO: might need to be told about password store dir
    postExec = "${pkgs.mu}/bin/mu index";
  };

  accounts.email = {
    maildirBasePath = "$HOME/Mail";
    accounts = {
      personal =
        gmailAccount
        {
          name = "personal";
          domain = "cdom.io";
        }
        // {
          primary = lib.mkDefault true;
          msmtp.enable = true;
        };
    };
  };

  programs.emacs = {
    extraPackages = epkgs: [
      pkgs.mu
    ];
  };
}
## References:
# https://github.com/Emiller88/dotfiles/blob/5eaabedf1b141c80a8d32e1b496055231476f65e/modules/shell/mail.nix
# https://github.com/berbiche/dotfiles/blob/cf8bc65bb775b69727a660a75ef2b981b0a31e54/profiles/email/accounts.nix

