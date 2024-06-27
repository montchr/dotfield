{ lib
, rustPlatform
, fetchFromGitHub
, stdenv
, darwin
}:

rustPlatform.buildRustPackage rec {
  pname = "shpool";
  version = "0.6.2";

  src = fetchFromGitHub {
    owner = "shell-pool";
    repo = "shpool";
    rev = "v${version}";
    hash = "sha256-6gfK71uM6IOP571Jzv3QPPKITaRteXyySZAstH0e+/M=";
    fetchSubmodules = true;
  };

  cargoHash = "sha256-rJ+Avq/6y68xEcJ+EeFVhFaSWJyC+x0a46cclVsTE4Q=";

  buildInputs = lib.optionals stdenv.isDarwin [
    darwin.apple_sdk.frameworks.CoreFoundation
    darwin.apple_sdk.frameworks.CoreServices
  ];

  meta = with lib; {
    description = "Think tmux, then aim... lower";
    homepage = "https://github.com/shell-pool/shpool";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
    mainProgram = "shpool";
  };
}
