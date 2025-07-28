{ pkgs, ... }:
let
  gmailAccount =
    {
      name,
      domain,
      realName ? "chris montgomery",
      username ? "chris",
      ...
    }:
    let
      address = "${username}@${domain}";
    in
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
        groups.${name}.channels =
          lib.mapAttrs
            (
              _: v:
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
              }
            )
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
in
{
  # accounts.email.accounts.personal =
  #   gmailAccount {
  #     name = "personal";
  #     domain = "cdom.io";
  #   }
  #   // {
  #     primary = true;
  #     msmtp.enable = true;
  #   };

}
