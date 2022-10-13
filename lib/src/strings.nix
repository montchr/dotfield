{l, ...}: {
  /*
  Converts the first character of an ASCII string to upper-case.

  Type: upperFirstChar :: string -> string

  Example:
    upperFirstChar "nixOS"
    => "NixOS"
  */
  upperFirstChar = s: l.toUpper (l.substring 0 1 s) + l.substring 1 (-1) s;
}
