# Copyright (C) 2024  Chris Montgomery
# Copyright (C) 2021  Jörg Thalheim
# SPDX-License-Identifier: GPL-3.0-or-later AND MIT
# SOURCE: <https://github.com/Mic92/dotfiles/blob/main/nixos/repl.nix>
# USAGE: nix repl ./repl.nix --argstr hostname <hostname>
let
  currentHostname = builtins.head (
    builtins.match "([a-zA-Z0-9]+)\n" (builtins.readFile "/etc/hostname")
  );
in
{
  hostname ? currentHostname,
}:
(builtins.getFlake (toString ./..)).nixosConfigurations.${hostname}
