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
{ l, pkgs }:
let
  darwinSystem = pkgs.stdenv.hostPlatform.system;
  linuxSystem = l.replaceStrings [ "darwin" ] [ "linux" ] darwinSystem;
  dataDir = "/var/lib/nixos-builder";
  port = 31022;
in
rec {
  inherit dataDir port;
  logPath = "/var/log/linux-builder.log";

  builder =
    (import "${pkgs.path}/nixos" {
      system = linuxSystem; # {x86_64|aarch64}-linux
      configuration =
        { modulesPath, lib, ... }:
        {
          imports = [ "${modulesPath}/profiles/macos-builder.nix" ];
          virtualisation = {
            host.pkgs = pkgs;
            forwardPorts = lib.mkForce [
              {
                from = "host";
                host.address = "127.0.0.1";
                host.port = port;
                guest.port = 22;
              }
            ];
          };
        };
    }).config.system.build.macos-builder-installer;

  builderMachine = {
    hostName = "ssh://linux-builder";
    maxJobs = 4;
    # This is cheating: KVM isn't actually available (?) but QEMU falls back to "slow mode" in this case
    supportedFeatures = [ "kvm" ];
    system = linuxSystem; # {x86_64|aarch64}-linux
  };

  script =
    let
      name = "run-linux-builder";
      bin = pkgs.writeShellScriptBin name ''
        set -uo pipefail
        trap 's=$?; echo "$0: Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
        IFS=$'\n\t'
        mkdir -p "${dataDir}"
        cd "${dataDir}"
        ${builder}/bin/create-builder
      '';
    in
    "${bin}/bin/${name}";
}
