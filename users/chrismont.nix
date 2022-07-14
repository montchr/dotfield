{
  config,
  pkgs,
  lib,
  suites,
  profiles,
  hmUsers,
  ...
}: {
  users.users.chrismont = {
    home = "/Users/montchr";
    isHidden = false;
    shell = pkgs.zsh;
  };

  home-manager.users.montchr = hmArgs: {
    imports =
      [hmUsers.xtallos]
      ++ (with hmArgs.suites; workstation)
      ++ (with hmArgs.profiles; [
        aws
        nodejs
        php
        ruby
        virtualisation.vagrant
      ]);

    home.packages = with pkgs; [
      ngrok
      # TODO: move this to a common profile when pandas dep is fixed upstream
      visidata # A terminal spreadsheet multitool for discovering and arranging data
    ];

    programs.firefox.profiles = {
      home.isDefault = false;
      work.isDefault = true;
    };

    programs.git.includes = [
      {
        condition = "gitdir:~/broadway/**";
        contents = {
          user.email = "chris@alley.co";
        };
      }
    ];
  };
}
