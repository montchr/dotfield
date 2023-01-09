# SPDX-FileCopyrightText Copyright (c) 2022-2023 Chris Montgomery
# SPDX-FileCopyrightText Copyright (c) 2017-2022 Home Manager contributors
# SPDX-LicenseIdentifier: GPL-3.0-or-later OR MIT
#
## Sources:
# - https://github.com/nix-community/home-manager/blob/bc90de24d898655542589237cc0a6ada7564cb6c/modules/programs/vscode.nix#L9
{
  config,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config) xdg;
  inherit (config.lib.file) mkOutOfStoreSymlink;

  # FIXME: don't hard-code flake root path
  configBasePath = "${xdg.configHome}/dotfield/home/users/cdom/config";
  configDir = "${configBasePath}/vscode";

  vscodePname = "vscodium";

  appConfigDir =
    {
      "vscode" = "Code";
      "vscode-insiders" = "Code - Insiders";
      "vscodium" = "VSCodium";
    }
    .${vscodePname};
  userDir =
    if isDarwin
    then "Library/Application Support/${appConfigDir}/User"
    else "${xdg.configHome}/${appConfigDir}/User";

  configFilePath = "${userDir}/settings.json";
  keybindingsFilePath = "${userDir}/keybindings.json";
  snippetsDirPath = "${userDir}/snippets";
  # tasksFilePath = "${userDir}/tasks.json";
in {
  home.file.${configFilePath}.source = mkOutOfStoreSymlink "${configDir}/settings.json";
  home.file.${keybindingsFilePath}.source = mkOutOfStoreSymlink "${configDir}/keybindings.json";
  home.file.${snippetsDirPath}.source = mkOutOfStoreSymlink "${configDir}/snippets";
}
