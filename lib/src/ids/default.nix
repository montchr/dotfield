{
  l,
  self,
  ...
}: {
  /*
  Type: genid_uint31 :: string -> int

  Source: <https://git.thalheim.io/Mic92/stockholm/src/commit/0b2952f4ed9572521f7c4a21904943ac33c602b0/lib/default.nix#L45>
  */
  genid_uint31 = x: ((self.ids.genid_uint32 x) + 16777216) / 2;

  /*
  Type: genid_uint31 :: string -> int

  Source: <https://git.thalheim.io/Mic92/stockholm/src/commit/0b2952f4ed9572521f7c4a21904943ac33c602b0/lib/default.nix#L46>
  */
  genid_uint32 = import ./genid.nix {inherit l self;};
}
