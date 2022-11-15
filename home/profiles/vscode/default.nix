# SPDX-FileCopyrightText Copyright (c) 2022 Chris Montgomery
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
  inherit (config.lib.file) mkOutOfStoreSymlink;

  vscodePname = "vscodium";

  configDir =
    {
      "vscode" = "Code";
      "vscode-insiders" = "Code - Insiders";
      "vscodium" = "VSCodium";
    }
    .${vscodePname};
  userDir =
    if isDarwin
    then "Library/Application Support/${configDir}/User"
    else "${config.xdg.configHome}/${configDir}/User";

  configFilePath = "${userDir}/settings.json";
  keybindingsFilePath = "${userDir}/keybindings.json";
  snippetsDirPath = "${userDir}/snippets";
  # tasksFilePath = "${userDir}/tasks.json";
in {
  home.file.${configFilePath}.source = mkOutOfStoreSymlink ./settings.json;
  home.file.${keybindingsFilePath}.source = mkOutOfStoreSymlink ./keybindings.json;
  home.file.${snippetsDirPath}.source = mkOutOfStoreSymlink ./snippets;
}
