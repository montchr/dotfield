{
  l,
  lib,
  ...
}: let
  inherit (lib.types) host;
  t = l.types;
in
  # TODO(cdom): consider reworking this if it doesn't fit with our approach
  t.submodule {
    options = {
      # TODO(stockholm): user
      host = l.mkOption {
        type = host;
      };
      # TODO(stockholm): merge with ssl.privkey.path
      path = l.mkOption {
        type = t.either t.path t.str;
        apply = x:
          {
            path = toString x;
            string = x;
          }
          .${l.typeOf x};
      };
    };
  }
