let
  /**
    Extract the value of a single attribute from an attrset, or return an empty attrset if none exists.

    This function is designed for attrsets that are known to have exactly zero or one attribute.
    If the input attrset has exactly one attribute, returns that attribute's value.
    If the input attrset is empty, returns an empty attrset.

    # Inputs

    `attrset`

    : An attribute set containing exactly zero or one attribute

    # Type

    getSingleValue :: AttrSet -> Any

    # Examples

    ```nix
    getSingleValue { foo = "bar"; }
    => "bar"

    getSingleValue {}
    => null

    getSingleValue { x = { a = 1; b = 2; }; }
    => { a = 1; b = 2; }
    ```
  */
  getSingleValue =
    attrset:
    let
      values = builtins.attrValues attrset;
    in
    if values == [ ] then null else builtins.head values;
in
{
  flake.lib.attrs = {
    inherit getSingleValue;
  };
}
