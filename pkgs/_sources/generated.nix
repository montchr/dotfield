# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub }:
{
  chemacs = {
    pname = "chemacs";
    version = "868388321169eddf6dcb99f9b0d3ce734897b3de";
    src = fetchgit {
      url = "https://github.com/plexus/chemacs2.git";
      rev = "868388321169eddf6dcb99f9b0d3ce734897b3de";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-XsJ2hHoQGoDbM7J+VvO1u0+f+jJCQqcUqQjzvTlnnG0=";
    };
  };
  fish-nvm = {
    pname = "fish-nvm";
    version = "2.2.7";
    src = fetchFromGitHub ({
      owner = "jorgebucaran";
      repo = "nvm.fish";
      rev = "2.2.7";
      fetchSubmodules = false;
      sha256 = "sha256-VOai2SITr4d6y8jGc27FqQ8HBG8H70ctWmg8pF4EFeQ=";
    });
  };
  fish-replay = {
    pname = "fish-replay";
    version = "1.2.0";
    src = fetchFromGitHub ({
      owner = "jorgebucaran";
      repo = "replay.fish";
      rev = "1.2.0";
      fetchSubmodules = false;
      sha256 = "sha256-Q/9YVdiRSJw1SdcfQv2h7Lj6EyFustRk+kmh1eRRQ6k=";
    });
  };
  fzf-scripts = {
    pname = "fzf-scripts";
    version = "7cf2925b0194f0ad116b84e8f45d8f01a87c774f";
    src = fetchgit {
      url = "https://github.com/DanielFGray/fzf-scripts";
      rev = "7cf2925b0194f0ad116b84e8f45d8f01a87c774f";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-wGCbc9jF0kS2EKLIDPkR8kokE65wGDt+RptBeJvBrnc=";
    };
  };
  kitty-bortflower-icons = {
    pname = "kitty-bortflower-icons";
    version = "b19b3969d3bdc870beeff332355f92e38dc5a704";
    src = fetchgit {
      url = "https://github.com/DinkDonk/kitty-icon.git";
      rev = "b19b3969d3bdc870beeff332355f92e38dc5a704";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-3Jri8/y/s6/QkOBb03R9nd1gyZPGZlekhli9ShTzOxg=";
    };
  };
}
