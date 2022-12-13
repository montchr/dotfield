{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;
in rec {
  /*
  Given a nested attrset and an attribute path, return the names of the
  top-level attributes whose attribute value at the given path matches a
  predicate.

  One use case may be comparing all home-manager configurations (i.e. source
  attrset) to determine whether a user (i.e. whose name may be returned in the
  resulting list) has a matching value (i.e. predicate) within a particular
  module (i.e. attrpath).

  Type: treesWithValue :: (String -> Any -> Bool) -> [String] -> AttrSet -> [String]

  Example:
    let
      users = {
        foo = { services.gpg-agent.pinentryFlavor = "gtk2"; };
        bar = { services.gpg-agent.pinentryFlavor = "gnome3"; };
      };
    in
    lib.treesWithValue (_: v: "gnome3" == v) ["services" "gpg-agent" "pinentryFlavor"] users
      => [ "bar" ]
  */
  treesWithValue = fn: path: attrs: (l.attrNames
    (l.filterAttrs
      (k: v: (fn k (l.getAttrFromPath path v)))
      attrs));

  /*
  Like `treesWithValue`, but with a pre-supplied no-op predicate to test a
  boolean value only.

  Type: treesWithEnabledLeaf :: [String] -> AttrSet -> [String]

  Example:
    let
      users = {
        foo = { programs.emacs.enable = true; };
        bar = { programs.emacs.enable = false; };
      };
    in
    lib.treesWithEnabledLeaf ["programs" "emacs" "enable"] users
      => [ "foo" ]
  */
  treesWithEnabledLeaf = treesWithValue (_: v: v);

  /*
  hasEnabledLeaf :: [String] -> AttrSet -> Bool

  Whether any of the top-level trees of the same structure have a module-like
  "enable" option whose value is true.

  Example:

  ```nix
  {config,...}:
  let
    hmUsers = config.home-manager.users;
    hasUserEmacs = hasEnabledLeaf ["programs" "emacs"] hmUsers;
  in
  {
    home-manager.users = {
      foo = { programs.emacs.enable = true; };
      bar = { programs.neovim.enable = true; };
    };
    programs.emacs.enable = hasUserEmacs;
  }
  ```
  */
  hasEnabledLeaf = path: attrs: let
    trees = treesWithEnabledLeaf (path ++ ["enable"]) attrs;
  in ((builtins.length trees) >= 1);
}
