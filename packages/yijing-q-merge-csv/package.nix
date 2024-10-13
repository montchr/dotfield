{ miller, writeShellApplication }:
let
  initialOrder = "datetime,method,setting,query,hex_primary,hex_relating,unknown_1,date,unknown_2,unknown_3,seq_primary,seq_relating,uuid";
  newOrder = "datetime,relevance,query,setting,hex_primary,seq_primary,hex_relating,seq_relating";
in
writeShellApplication {
  name = "yijing-q-merge-csv";
  runtimeInputs = [ miller ];
  text = ''
    DIR=''${1:-$HOME/Documents/data/yijing/app}

    mlr --csv \
      --ifs ';' \
      --lazy-quotes \
      --implicit-csv-header \
      label ${initialOrder} \
      then cat \
      then sort -f datetime \
      then uniq -a -c \
      then rename count,relevance \
      then reorder -f ${newOrder} \
      $DIR/_raw/YiJing-Q-* \
      > $DIR/yijing-q.merged.csv
  '';
}
