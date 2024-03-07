# SPDX-FileCopyrightText: 2023 Chris Montgomery <chris@cdom.io>
# SPDX-FileCopyrightText: 2022-2023 Luc Perkins <lucperkins@gmail.com>
# SPDX-License-Identifier: GPL-3.0-or-later OR MPL-2.0
#
# This file is free software: you may copy, redistribute and/or modify
# it under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version XX of the License, or (at your
# option) any later version.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/.
#
# Copyright (c) 2023 Chris Montgomery <chris@cdom.io>
#
# This file incorporates work covered by the following copyright and
# permission notice:
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.
#
# Copyright (c) 2022-2023 Luc Perkins <lucperkins@gmail.com>
{ flake, pkgs, ... }:
let
  l = flake.inputs.nixpkgs.lib // builtins;
  linuxBuilder = import ./machine.nix { inherit l pkgs; };
in
{
  environment.etc."nix/ssh_config".text = ''
    Host linux-builder
      User builder
      HostName 127.0.0.1
      Port ${toString linuxBuilder.port}
      IdentityFile ${linuxBuilder.dataDir}/keys/builder_ed25519
      UserKnownHostsFile /etc/nix/ssh_known_hosts
  '';

  environment.etc."nix/ssh_known_hosts".text = ''
    [127.0.0.1]:31022 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJBWcxb/Blaqt1auOtE+F8QUWrUotiC5qBJ+UuEWdVCb
  '';

  launchd.daemons.linux-builder = {
    command = linuxBuilder.script;
    path = with pkgs; [
      "/usr/bin"
      coreutils
      nix
    ];
    serviceConfig = {
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = linuxBuilder.logPath;
      StandardErrorPath = linuxBuilder.logPath;
    };
  };

  nix = {
    buildMachines = [ linuxBuilder.builderMachine ];
    distributedBuilds = true;
    envVars = {
      NIX_SSHOPTS = "-F /etc/nix/ssh_config";
    }; # See the config above
  };

  # Make sure that the Nix daemon is enabled in the nix-darwin config
  services.nix-daemon.enable = true;
}
