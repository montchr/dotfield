{
  stdenv,
  lib,
  nerd-font-patcher,
  iosevka ?
    iosevka.override {
      privateBuildPlan = {
        family = "Iosevka Seadome";
        spacing = "normal";
        serifs = "sans";
        no-cv-ss = false;
        no-litigation = false;
      };
      set = "seadome";
      variants = {
        inherits = "ss08";
      };
      ligations = {
        inherits = "dlig";
      };
    },
}:
stdenv.mkDerivation {
  pname = "iosevka-seadome";
  version = iosevka.version;

  nativeBuildInputs = [nerd-font-patcher iosevka];

  phases = ["installPhase"];

  preInstall = ''
    mkdir -p $out/share/fonts/truetype && cd "$_"
  '';

  installPhase = ''
    runHook preInstall

    find ${iosevka}/share/fonts/truetype \
      -name \*.ttf \
      -exec ${nerd-font-patcher}/bin/nerd-font-patcher --complete --quiet --no-progressbars {} \; \
      -exec ${nerd-font-patcher}/bin/nerd-font-patcher --complete --adjust-line-height --quiet --no-progressbars {} \;
  '';

  meta = with lib; {
    description = "Versatile typeface for code, from code";
    homepage = "https://be5invis.github.io/Iosevka";
    license = licenses.ofl;
    maintainers = with maintainers; [];
    platforms = platforms.all;
  };
}
