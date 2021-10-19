{ src }: final: prev: {
  emacs = (prev.emacs.overrideAttrs (o: rec {
    inherit src;
    version = "28.0.60";
  }));
}
