{ lib, self, ... }:
let
  isPrimary = value: value.primary or false;

  isEmpty =
    v: (v == "") || (v == false) || (v == null) || (v == { }) || (v == [ ]) || (v == 0) || (v == 0.0);
in
{
  flake.lib = {
    inherit isEmpty isPrimary;

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
