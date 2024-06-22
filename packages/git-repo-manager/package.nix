{
  lib,
  rustPlatform,
  fetchFromGitHub,
  curl,
  pkg-config,
  libgit2,
  openssl,
  zlib,
  stdenv,
  darwin,
}:

rustPlatform.buildRustPackage rec {
  pname = "git-repo-manager";
  version = "0.7.15";

  src = fetchFromGitHub {
    owner = "hakoerber";
    repo = "git-repo-manager";
    rev = "v${version}";
    hash = "sha256-NSr8wWBKwT8AOO/NSHhSnYeje2hkAyjtDwooa2RqTfE=";
  };

  cargoHash = "sha256-Xnue2oJ5Y0VVa1raXM703oQuFegh4wSoPxd26bVcjnQ=";

  nativeBuildInputs = [
    curl
    pkg-config
  ];

  buildInputs = [
    curl
    libgit2
    openssl
    zlib
  ] ++ lib.optionals stdenv.isDarwin [ darwin.apple_sdk.frameworks.Security ];

  env = {
    OPENSSL_NO_VENDOR = true;
  };

  meta = with lib; {
    description = "A git tool to manage worktrees and integrate with GitHub and GitLab";
    homepage = "https://github.com/hakoerber/git-repo-manager";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ ];
    mainProgram = "git-repo-manager";
  };
}
