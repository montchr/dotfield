{
  lib,
  rustPlatform,
  fetchFromGitLab,
}:
rustPlatform.buildRustPackage rec {
  pname = "starship-jj";
  version = "0.4.1";

  src = fetchFromGitLab {
    owner = "lanastara_foss";
    repo = "starship-jj";
    rev = version;
    hash = "sha256-gV0YerQrOt15Z781pg5dPnkZqxyXV8KP8zlzU5wC5SI=";
  };

  cargoHash = "sha256-Fm3RHNPdq9SIt6wFRlPWTTyCrfaDAAdLp96rC53H4lI=";

  meta = {
    description = "Plugin for Starship prompt providing support for the Jujutsu version control system";
    homepage = "https://gitlab.com/lanastara_foss/starship-jj";
    changelog = "https://gitlab.com/lanastara_foss/starship-jj/-/blob/${src.rev}/CHANGELOG.md";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "starship-jj";
  };
}
