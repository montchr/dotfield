# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub }:
{
  HPI = {
    pname = "HPI";
    version = "0.3.20220607";
    src = fetchurl {
      url = "https://pypi.io/packages/source/H/HPI/HPI-0.3.20220607.tar.gz";
      sha256 = "sha256-GME0Z+TH/6U+smFkuk1sT8UBzlySv5/yGhb42Kiaj8w=";
    };
  };
  cpanel-cli = {
    pname = "cpanel-cli";
    version = "c2419f32ebf31fa4f7122ddf110df0a8b9e44925";
    src = fetchFromGitHub ({
      owner = "layfellow";
      repo = "cpanel-cli";
      rev = "c2419f32ebf31fa4f7122ddf110df0a8b9e44925";
      fetchSubmodules = false;
      sha256 = "sha256-EhlL6xamqvTqCAq8k1XrxzgbqhdpG0TpFVYzJSNqpfE=";
    });
  };
  firefox-lepton-ui = {
    pname = "firefox-lepton-ui";
    version = "v6.0.0";
    src = fetchFromGitHub ({
      owner = "black7375";
      repo = "Firefox-UI-Fix";
      rev = "v6.0.0";
      fetchSubmodules = false;
      sha256 = "sha256-w4kgupQEhtZMVYJ4JR5aJf+BSa25i4ksIYDY9QaQyr8=";
    });
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
  hug = {
    pname = "hug";
    version = "2.6.1";
    src = fetchurl {
      url = "https://pypi.io/packages/source/h/hug/hug-2.6.1.tar.gz";
      sha256 = "sha256-sO2s4qy2GIc3ecnObs+RZdtU/vlcIiYvVwD83Z/rrsk=";
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
  orgparse = {
    pname = "orgparse";
    version = "0.3.1";
    src = fetchurl {
      url = "https://pypi.io/packages/source/o/orgparse/orgparse-0.3.1.tar.gz";
      sha256 = "sha256-hg5vu5pnt0K6p5LmD4zBhSLpeJwGXSaCHAIoXV/BBK8=";
    };
  };
  promnesia = {
    pname = "promnesia";
    version = "346b2e08e04604adffb17ae51244cd1b1ec9015f";
    src = fetchFromGitHub ({
      owner = "karlicoss";
      repo = "promnesia";
      rev = "346b2e08e04604adffb17ae51244cd1b1ec9015f";
      fetchSubmodules = false;
      sha256 = "sha256-GcLPJZQaHqPUGvcZMNlofwqTizh5/PywA4vY9N3Ih7s=";
    });
  };
  roots-trellis-cli = {
    pname = "roots-trellis-cli";
    version = "v1.7.0";
    src = fetchFromGitHub ({
      owner = "roots";
      repo = "trellis-cli";
      rev = "v1.7.0";
      fetchSubmodules = false;
      sha256 = "sha256-/YUoGPVTbmegr8cmjhtHnG1jiwIjen91sUWjgK3T8GQ=";
    });
  };
}
