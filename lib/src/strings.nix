{lib, ...}: {
  /*
  Converts the first character of an ASCII string to upper-case.

  Type: upperFirstChar :: string -> string

  Example:
    upperFirstChar "nixOS"
    => "NixOS"
  */
  upperFirstChar = s: lib.toUpper (lib.substring 0 1 s) + lib.substring 1 (-1) s;
}
