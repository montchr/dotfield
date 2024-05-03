# FIXME: failing tests (possibly due to attempting to write files?)

{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "tinty";
  version = "0.12.0";

  src = fetchFromGitHub {
    owner = "tinted-theming";
    repo = "tinty";
    rev = "v${version}";
    hash = "sha256-p5p+7MdMB/uALJ1I9AKuykl07vlxX6f4cRFu18Fzg90=";
  };

  cargoHash = "sha256-We5LC9ABBIqP9rTYu4R9+S7OFtiyhGzOxshrYuEHnDY=";

  meta = with lib; {
    description = "A base16 and base24 color scheme manager";
    homepage = "https://github.com/tinted-theming/tinty";
    changelog = "https://github.com/tinted-theming/tinty/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    mainProgram = "tinty";
  };
}
