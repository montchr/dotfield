{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "iching";
  version = "unstable-2023-02-02";

  src = fetchFromGitHub {
    owner = "Velfi";
    repo = "i-ching";
    rev = "ac21a0a4646b6a4a2e9b8599bd3231e40a80d68a";
    hash = "sha256-qrv1t4UDEeluqoKEzG9WdgDh3VYFpQ67dDQTad4llhk=";
  };

  cargoHash = "sha256-WLZ5EfWwm12C3I0+NZc6TgmRctIAqDTM5hJalJUFGm0=";

  meta = {
    description = "A library for i-ching apps written in Rust, and a companion CLI app for divination";
    homepage = "https://github.com/Velfi/i-ching";
    license = with lib.licenses; [
      asl20
      mit
    ];
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "iching";
  };
}
