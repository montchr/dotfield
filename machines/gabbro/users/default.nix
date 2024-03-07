{ ops, ... }:
{
  imports = [ ./cdom.nix ];

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";

  security.sudo.enable = true;
  security.sudo.wheelNeedsPassword = false;

  users.users.root.openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
}
