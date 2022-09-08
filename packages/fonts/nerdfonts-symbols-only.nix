{
  stdenv,
  fetchurl,
  lib,
  unzip,
}:
let
  version = "2.2.1";
  pname = "nerdfonts-symbols-only";
in
stdenv.mkDerivation {
  inherit pname version;

  src = fetchurl {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v${version}/NerdFontsSymbolsOnly.zip";
    hash = "sha256-mLhcPVoNXWXKoZYleptkduXL5U5fIeWxMkLJ6MgFvWM=";
  };

  nativeBuildInputs = [unzip];
  sourceRoot = ".";
  installPhase = ''
    find -name \*.otf -exec mkdir -p $out/share/fonts/opentype/NerdFonts \; -exec mv {} $out/share/fonts/opentype/NerdFonts \;
    find -name \*.ttf -exec mkdir -p $out/share/fonts/truetype/NerdFonts \; -exec mv {} $out/share/fonts/truetype/NerdFonts \;
    rm -rfv $out/share/fonts/opentype/NerdFonts/*Windows\ Compatible.*
    rm -rfv $out/share/fonts/truetype/NerdFonts/*Windows\ Compatible.*
  '';

  meta = with lib; {
    description = "Iconic font aggregator, collection, & patcher. 3,600+ icons, 50+ patched fonts";
    longDescription = ''
      Nerd Fonts is a project that attempts to patch as many developer targeted
      and/or used fonts as possible. The patch is to specifically add a high
      number of additional glyphs from popular 'iconic fonts' such as Font
      Awesome, Devicons, Octicons, and others.
    '';
    homepage = "https://nerdfonts.com/";
    license = licenses.mit;
    maintainers = ["montchr"];
    hydraPlatforms = []; # 'Output limit exceeded' on Hydra
  };
}
