# SPDX-FileCopyrightText Copyright (c) 2022-2023 Chris Montgomery
# SPDX-LicenseIdentifier: GPL-3.0-or-later
{
  #  imports = [ ./user-settings.nix ];
  programs.vscode = {
    enable = true;
    enableExtensionUpdateCheck = true;
    enableUpdateCheck = false;
    # keybindings = import ./keybindings.nix;
    mutableExtensionsDir = true;
  };
}
