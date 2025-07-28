# Copyright (c) 2018–2021 Robert Helgesson
# SPDX-License-Identifier: MIT
#
# ddi -- "nice dd setup for most cases"
#
# https://git.sr.ht/~rycee/configurations/tree/1af2ef3d4c8778b0fb2b12934d3a3f1766ce1d9f/item/user/common.nix#L62-66
{ coreutils, writeShellScriptBin }:
(writeShellScriptBin "ddi" ''
  ${coreutils}/bin/dd if="$1" of="$2" \
    bs=128k iflag=nocache oflag=direct conv=fsync status=progress
'')
