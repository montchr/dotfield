{l, ...}: {
  maxRatio = h: "bestvideo[height<=?${l.toString h}][fps<=?30]+bestaudio/best[height<=?${
    l.toString h
  }][fps<=?30]";
}
