###: <https://tools.ietf.org/html/rfc5952>
###: via <https://git.thalheim.io/Mic92/stockholm/src/commit/0b2952f4ed9572521f7c4a21904943ac33c602b0/lib/default.nix#L107-L162>
{inputs, ...}: let
  l = inputs.nixpkgs.lib // builtins;

  max-run-0 = let
    both = v: {
      off = v;
      pos = v;
    };
    gt = a: b: a.pos - a.off > b.pos - b.off;

    chkmax = ctx: {
      cur = both (ctx.cur.pos + 1);
      max =
        if gt ctx.cur ctx.max
        then ctx.cur
        else ctx.max;
    };

    incpos = ctx:
      l.recursiveUpdate ctx {
        cur.pos = ctx.cur.pos + 1;
      };

    f = ctx: blk:
      (
        if blk == "0"
        then incpos
        else chkmax
      )
      ctx;
    z = {
      cur = both 0;
      max = both 0;
    };
  in
    blks: (chkmax (l.foldl' f z blks)).max;

  group-zeros = a: let
    blks = l.splitString ":" a;
    max = max-run-0 blks;
    lhs = l.take max.off blks;
    rhs = l.drop max.pos blks;
  in
    if max.pos == 0
    then a
    else let
      sep =
        if 8 - (l.length lhs + l.length rhs) == 1
        then ":0:"
        else "::";
    in "${l.concatStringsSep ":" lhs}${sep}${l.concatStringsSep ":" rhs}";

  drop-leading-zeros = let
    f = block: let
      res = l.match "0*(.+)" block;
    in
      if res == null
      then block # empty block
      else l.elemAt res 0;
  in
    a: l.concatStringsSep ":" (l.map f (l.splitString ":" a));
in
  a:
    l.toLower (
      if l.test ".*::.*" a
      then a
      else group-zeros (drop-leading-zeros a)
    )
