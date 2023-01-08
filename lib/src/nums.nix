_: {
  /*
  Type: mod :: int -> int

  Source: <https://git.thalheim.io/Mic92/stockholm/src/commit/0b2952f4ed9572521f7c4a21904943ac33c602b0/lib/default.nix#L40>
  */
  mod = x: y: x - y * (x / y);

  /*
  Convert an integer to a floating-point number.

  Type: toFloat :: number -> float
  */
  toFloat = x: x + 0.0;
}
