{l, ...}: let
  t = l.types;
in {
  binary-cache-pubkey = t.str;
  pgp-pubkey = t.str;
  ssh-pubkey = t.str;
  ssh-privkey = t.submodule {
    options = {
      bits = l.mkOption {
        type = t.nullOr (t.enum ["4096"]);
        default = null;
      };
      path = l.mkOption {
        type = t.either t.path t.str;
        apply = x:
          {
            path = l.toString x;
            string = x;
          }
          .${l.typeOf x};
      };
      type = l.mkOption {
        type = t.enum ["rsa" "ed25519"];
        default = "ed25519";
      };
    };
  };
  wireguard-pubkey = t.str;
}
