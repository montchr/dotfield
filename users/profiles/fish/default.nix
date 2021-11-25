{ config, lib, pkgs, ... }:

let
  inherit (lib.strings) fileContents;

  shellCfg = config.my.modules.shell;
  configDir = "${config.dotfield.configDir}/fish";

  mkPlugins = plugins: (map
    (name: {
      inherit name;
      inherit (pkgs.sources."fish-${name}") src;
    })
    plugins);

  mkFileLink = path: onChange: {
    "fish/${path}" = {
      inherit onChange;
      source = "${configDir}/${path}";
    };
  };
  mkFileLink' = path: mkFileLink "${path}.fish";
in
{
  imports = [
    ../shell
  ];

  my.hm.programs = {
    fish = {
      enable = true;
      interactiveShellInit = fileContents ./interactiveShellInit.fish;
      shellInit = fileContents ./shellInit.fish;
      shellAbbrs = shellCfg.abbrs;
      shellAliases = shellCfg.aliases;
      plugins = mkPlugins [
        "abbr-tips"
        "autopair"
        "done"
        "fzf"
        "nix-env"
        "nvm"
        "replay"
        "z"
      ];
    };
    starship.enableFishIntegration = true;
  };
}
