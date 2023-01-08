{
  l,
  lib,
  ...
}: let
  inherit (lib.strings) test;
in rec {
  # RFC952, B. Lexical grammar, <hname>
  hostname = l.mkOptionType {
    name = "hostname";
    check = x: l.isString x && l.all label.check (l.splitString "." x);
    merge = l.mergeOneOption;
  };

  # RFC952, B. Lexical grammar, <name>
  # RFC1123, 2.1  Host Names and Numbers
  label = l.mkOptionType {
    name = "label";
    # TODO(stockholm): case-insensitive labels
    check = test "[0-9A-Za-z]([0-9A-Za-z-]*[0-9A-Za-z])?";
    merge = l.mergeOneOption;
  };

  # POSIX.1-2017, 3.216 Login Name
  username = l.mkOptionType {
    inherit (filename) check;
    name = "POSIX login name";
    merge = l.mergeOneOption;
  };

  # POSIX.1‐2017, 3.281 Portable Filename
  filename = l.mkOptionType {
    name = "POSIX portable filename";
    check = test "[0-9A-Za-z._][0-9A-Za-z._-]*";
    merge = l.mergeOneOption;
  };

  # POSIX.1‐2017, 3.2 Absolute Pathname
  absolute-pathname = l.mkOptionType {
    name = "POSIX absolute pathname";
    check = x: l.isString x && l.substring 0 1 x == "/" && pathname.check x;
    merge = l.mergeOneOption;
  };

  # POSIX.1-2017, 3.271 Pathname
  pathname = l.mkOptionType {
    name = "POSIX pathname";
    check = x: let
      # The filter is used to normalize paths, i.e. to remove duplicated and
      # trailing slashes.  It also removes leading slashes, thus we have to
      # check for "/" explicitly below.
      xs = l.filter (s: l.stringLength s > 0) (l.splitString "/" x);
    in
      l.isString x && (x == "/" || (l.length xs > 0 && l.all filename.check xs));
    merge = l.mergeOneOption;
  };
}
