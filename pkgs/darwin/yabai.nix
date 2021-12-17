{ stdenv, fetchzip, lib }:

stdenv.mkDerivation {
  pname = "yabai";
  version = "4.0.0-pre";

  src = fetchzip {
    url = "https://github.com/koekeishiya/yabai/files/7726893/yabai-v4.0.0.tar.gz";
    hash = "sha256-Gm0WH6saD29bOoMyTDbYVi7e2IP9eBAvib0hWDkJXVQ=";
  };

  # phases = [ "unpackPhase" "installPhase" ];
  # pathsToLink = [ "/share/fonts/opentype/" ];
  # sourceRoot = ".";
  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/share/man/man1/
    cp ./bin/yabai $out/bin/yabai
    cp ./doc/yabai.1 $out/share/man/man1/yabai.1
  '';

  meta = with lib; {
    description = ''
      A tiling window manager for macOS based on binary space partitioning
    '';
    homepage = "https://github.com/koekeishiya/yabai";
    platforms = platforms.darwin;
    # maintainers = with maintainers; [ cmacrae shardy ];
    license = licenses.mit;
  };
}
