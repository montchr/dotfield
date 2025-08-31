{ lib, self, ... }:
let
  isPrimary = value: value.primary or false;
in
{
  flake.lib = {
    inherit isPrimary;

    filterPrimary =
      value:
      if lib.isAttrs value then
        value |> lib.filterAttrs (_: isPrimary) |> self.lib.attrs.getSingleValue
      else if lib.isList value then
        value |> lib.filter isPrimary |> lib.head
      else
        throw "Unsupported type";
  };
}
