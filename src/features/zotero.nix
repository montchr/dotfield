# SPDX-FileCopyrightText: (C) 2026 Chris Montgomery <chmont@protonmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  aspects.workstation.nixos = { pkgs, ... }: {
    # HACK: Zotero package must be installed at system level
    #       so its insecure Firefox ESR 140 dependency can be allowlisted.
    #       Home Manager does not seem to respect the
    #       `permittedInsecurePackages` setting.
    environment.systemPackages = [
      pkgs.zotero
      pkgs.zotero-translation-server
    ];
  };

  aspects.workstation.home = { lib, pkgs, ... }: {
    # Make the bundled integration extensions available at a stable path for
    # integration targets.
    #
    # For more background on this issue, see:
    # <https://wiki.nixos.org/wiki/Zotero>
    xdg.dataFile."zotero/lib/integration".source = "${pkgs.zotero}/lib/integration";
  };
}
