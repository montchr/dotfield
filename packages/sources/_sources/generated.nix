# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub }:
{
  firefox-lepton-ui = {
    pname = "firefox-lepton-ui";
    version = "edf06317653e9294983144b485701658c79d88c9";
    src = fetchgit {
      url = "https://github.com/black7375/Firefox-UI-Fix";
      rev = "edf06317653e9294983144b485701658c79d88c9";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-2rVY270KYy+Xf79XxaGlOzTbkp9uDQORlNbWf1YZGEo=";
    };
  };
  fish-autopair = {
    pname = "fish-autopair";
    version = "4d1752ff5b39819ab58d7337c69220342e9de0e2";
    src = fetchgit {
      url = "https://github.com/jorgebucaran/autopair.fish";
      rev = "4d1752ff5b39819ab58d7337c69220342e9de0e2";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-qt3t1iKRRNuiLWiVoiAYOu+9E7jsyECyIqZJ/oRIT1A=";
    };
  };
  fish-fifc = {
    pname = "fish-fifc";
    version = "adff5966739667d4c13d6388372e40f821571208";
    src = fetchgit {
      url = "https://github.com/gazorby/fifc";
      rev = "adff5966739667d4c13d6388372e40f821571208";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-tUhEfwVtcd1iSHsmkOzkB5B33qK+x/AZ56Dgs8QEaDk=";
    };
  };
  fish-nix-env = {
    pname = "fish-nix-env";
    version = "7b65bd228429e852c8fdfa07601159130a818cfa";
    src = fetchgit {
      url = "https://github.com/lilyball/nix-env.fish";
      rev = "7b65bd228429e852c8fdfa07601159130a818cfa";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-RG/0rfhgq6aEKNZ0XwIqOaZ6K5S4+/Y5EEMnIdtfPhk=";
    };
  };
  fish-replay = {
    pname = "fish-replay";
    version = "bd8e5b89ec78313538e747f0292fcaf631e87bd2";
    src = fetchgit {
      url = "https://github.com/jorgebucaran/replay.fish";
      rev = "bd8e5b89ec78313538e747f0292fcaf631e87bd2";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-bM6+oAd/HXaVgpJMut8bwqO54Le33hwO9qet9paK1kY=";
    };
  };
  fzf-scripts = {
    pname = "fzf-scripts";
    version = "15156e3cb56c715464a2421e6f4e4356a26ac975";
    src = fetchgit {
      url = "https://github.com/DanielFGray/fzf-scripts";
      rev = "15156e3cb56c715464a2421e6f4e4356a26ac975";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-rynePmia169HOvL0M2GTWrndulS6dKjfx7rT0GK9J0I=";
    };
  };
  kitty-bortflower-icons = {
    pname = "kitty-bortflower-icons";
    version = "269c0f0bd1c792cebc7821f299ce9250ed9bcd67";
    src = fetchgit {
      url = "https://github.com/DinkDonk/kitty-icon.git";
      rev = "269c0f0bd1c792cebc7821f299ce9250ed9bcd67";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-Vy+iLGnysrJMSLfkaYq15pb/wG4kIbfsXRrPgSc3OFs=";
    };
  };
  promnesia = {
    pname = "promnesia";
    version = "47ae8c654bbba8e506c2c0e852ff0d4763d42869";
    src = fetchgit {
      url = "https://github.com/karlicoss/promnesia";
      rev = "47ae8c654bbba8e506c2c0e852ff0d4763d42869";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-MpgXhDQ8J9hd+t8mKAP3DufFsibmE+lo6A34ZrXnuMk=";
    };
  };
}
