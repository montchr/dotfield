{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:
rustPlatform.buildRustPackage rec {
  pname = "ast-grep";
  version = "0.5.1";

  src = fetchFromGitHub {
    owner = "ast-grep";
    repo = "ast-grep";
    rev = "v${version}";
    hash = "sha256-+0/kMJInN0jhs20WMk8lWHoo3IlvgTpsROdNduOy0j0=";
  };

  cargoHash = "sha256-MPnPv2xW2wUzPQwZp95EMLjWsHCI9TtH3R0H5HwHMAE=";

  meta = with lib; {
    mainProgram = "sg";
    description = "A fast and polyglot tool for code searching, linting, rewriting at large scale";
    homepage = "https://ast-grep.github.io/";
    changelog = "https://github.com/ast-grep/ast-grep/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ montchr ];
  };
}
