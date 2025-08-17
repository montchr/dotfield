{
  stdenv,
  lib,
  fetchurl,
  fetchzip,
  zstd,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "iosvmata-bin";
  version = "1.2.0";

  src = fetchzip {
    url = "https://github.com/N-R-K/Iosvmata/releases/download/v${finalAttrs.version}/Iosvmata-v${finalAttrs.version}.tar.zst";
    sha256 = "sha256-CvIXUbnxqVPV6rf0vr2rMQojvgMyqPPQIO7Cr8ItTgg=";
    nativeBuildInputs = [ zstd ];
  };

  installPhase = ''
    install -m444 -Dt $out/share/fonts/truetype Normal/*.ttf
  '';

  meta = {
    description = "Custom Iosevka build somewhat mimicking PragmataPro";
    homepage = "https://github.com/N-R-K/Iosvmata";
    # NOTE: The project does not appear to include a license.  However,
    # Iosevka is licensed under the SIL Open Font License (OFL). The OFL
    # is a copyleft license requiring that derivative works also be
    # distributed under the OFL.  For that reason, it is safe to make
    # the claim that this project is de facto licensed under the OFL.
    license = lib.licenses.ofl;
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "iosvmata";
    platforms = lib.platforms.all;
  };
})
