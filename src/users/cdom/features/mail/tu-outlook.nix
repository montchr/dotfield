{
  # FIXME: noop -- see notes below
  # users.cdom.aspects.mail.home =
  users.cdom.aspects.noop.home =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    {
      accounts.email.accounts."TU" = {
        address = "chrismont@temple.edu";
        realName = "chris montgomery";
        userName = "tuc26080@temple.edu";
        passwordCommand = "${lib.getExe pkgs.pizauth} show TU";
        # FIXME: needs `host`
        imap = {
          port = 993;
          tls.enable = true;
        };
        mbsync = {
          enable = true;
          create = "maildir";
          extraConfig.account.AuthMechs = "XOAUTH2";
          remove = "none";
          expunge = "none";
        };
        notmuch.enable = true;
        msmtp = {
          enable = true;
          extraConfig = {
            # account = "TU";
            auth = "xoauth2";
            host = "smtp.office365.com";
            protocol = "smtp";
            port = "587";
            tls = "on";
            tls_starttls = "on";
            from = "chrismont@temple.edu";
            user = "tuc26080@temple.edu";
            passwordeval = "pizauth show TU";
          };
        };
      };
    };
}
