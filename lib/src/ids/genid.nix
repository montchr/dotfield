###: Source: <https://git.thalheim.io/Mic92/stockholm/src/commit/0b2952f4ed9572521f7c4a21904943ac33c602b0/lib/genid.nix>
{
  l,
  self,
}: let
  inherit (self.num) mod;

  out = genid;

  /*
  id = genid s = (hash s + min) % max
  min <= genid s < max

  min = 2^24 =   16777216 = 0x001000000
  max = 2^32 = 4294967296 = 0x100000000

  id is bigger than UID of nobody and GID of nogroup
  see <nixos/modules/misc/ids.nix> and some spare for stuff like lxd.

  Type: genid :: str -> uint32
  */
  genid = s: sum16 (addmod16_16777216 (hash s));

  /*
  Type: hash :: str -> list8 uint4
  */
  hash = s:
    l.map hexint (l.stringToCharacters (l.substring 32 8 (l.hashString "sha1" s)));

  /*
  Type: sum16 :: list uint -> uint
  */
  sum16 = l.foldl (a: i: a * 16 + i) 0;

  /*
  Type: addmod16_16777216 :: list8 uint4 -> list1 uint8 ++ list6 uint4
  */
  addmod16_16777216 = x: let
    a = 16 * l.head x + l.head (l.tail x);
    d = l.tail (l.tail x);
  in
    [(mod (a + 1) 256)] ++ d;

  /*
  Type: hexint :: char -> uint4
  */
  hexint = x: hexvals.${l.toLower x};

  /*
  Type: hexvals :: attrset char uint4
  */
  hexvals = l.listToAttrs (l.imap (i: c: {
      name = c;
      value = i - 1;
    })
    (l.stringToCharacters "0123456789abcdef"));
in
  out
