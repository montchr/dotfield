{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:
buildGoModule rec {
  pname = "gh-repo-explore";
  version = "0.0.7";

  src = fetchFromGitHub {
    owner = "samcoe";
    repo = "gh-repo-explore";
    rev = "v${version}";
    hash = "sha256-l7FjcxXd7I0TWgHp39YGUWEo/jUX75TFV9iSf6SkYuE=";
  };

  vendorHash = "sha256-YTIRzKOjG21YeYYNLtjqQhEuDxbmTtx6+FUtf9zMyhQ=";

  ldflags = ["-s" "-w"];

  meta = with lib; {
    description = "GitHub CLI extension to interactively explore a repo without cloning";
    homepage = "https://github.com/samcoe/gh-repo-explore";
    license = licenses.mit;
    maintainers = with maintainers; [montchr];
    mainProgram = "gh-repo-explore";
  };
}
