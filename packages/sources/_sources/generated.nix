# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub }:
{
  firefox-lepton-ui = {
    pname = "firefox-lepton-ui";
    version = "0f1c64eeff063638a481737857cdfa9e0a57609a";
    src = fetchgit {
      url = "https://github.com/black7375/Firefox-UI-Fix";
      rev = "0f1c64eeff063638a481737857cdfa9e0a57609a";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-BJRt46PZ3LxoNkL0kmdcRuibOJ6+6N4DRAZ3TtJp3Ms=";
    };
  };
  fish-autopair = {
    pname = "fish-autopair";
    version = "1.0.4";
    src = fetchFromGitHub ({
      owner = "jorgebucaran";
      repo = "autopair.fish";
      rev = "1.0.4";
      fetchSubmodules = false;
      sha256 = "sha256-s1o188TlwpUQEN3X5MxUlD/2CFCpEkWu83U9O+wg3VU=";
    });
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
    version = "1.2.1";
    src = fetchFromGitHub ({
      owner = "jorgebucaran";
      repo = "replay.fish";
      rev = "1.2.1";
      fetchSubmodules = false;
      sha256 = "sha256-bM6+oAd/HXaVgpJMut8bwqO54Le33hwO9qet9paK1kY=";
    });
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
    src = fetchFromGitHub ({
      owner = "karlicoss";
      repo = "promnesia";
      rev = "47ae8c654bbba8e506c2c0e852ff0d4763d42869";
      fetchSubmodules = false;
      sha256 = "sha256-d1lbn92fzHajRYOIOoT6pbR+hc+vks+aLHjmJvfPEAs=";
    });
  };
  trellis-cli = {
    pname = "trellis-cli";
    version = "v1.8.0";
    src = fetchFromGitHub ({
      owner = "roots";
      repo = "trellis-cli";
      rev = "v1.8.0";
      fetchSubmodules = false;
      sha256 = "sha256-w02vxf9PZDuHbrItKOYG1U7tUWnFMNKeJRjWa2FNz+c=";
    });
  };
}
