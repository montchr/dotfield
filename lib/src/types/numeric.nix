{l, ...}: {
  uint = l.mkOptionType {
    name = "unsigned integer";
    check = x: l.isInt x && x >= 0;
    merge = l.mergeOneOption;
  };
}
