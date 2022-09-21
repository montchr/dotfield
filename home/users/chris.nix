# FIXME: non-functional
{pkgs, ...}: {
  users.users.chris = {
    isHidden = false;
    shell = pkgs.zsh;
  };

  home-manager.users.chris = hmArgs: {
    # imports = [hmUsers.chrismont];

    home.packages = with pkgs; [
      # TODO: move this to a common profile when pandas dep is fixed upstream
      visidata # A terminal spreadsheet multitool for discovering and arranging data
    ];

    programs.firefox.profiles = {
      home.isDefault = false;
      work.isDefault = true;
    };

    programs.git.includes = [
      {
        # FIXME: update to point to correct path after setting up gpg
        condition = "gitdir:~/workspaces/kleinweb**";
        contents = {
          # FIXME: add this email to pgp key
          # TODO: configure mail
          user.email = "chrismont@temple.edu";
        };
      }
    ];
  };
}
