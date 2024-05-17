# FIXME: remove when available in a release: <https://github.com/gazorby/fifc/issues/31>
{
  lib,
  fishPlugins,
  fetchFromGitHub,
}:
fishPlugins.buildFishPlugin rec {
  pname = "fifc";
  version = "unstable-2024-04-07";

  src = fetchFromGitHub {
    owner = "gazorby";
    repo = "fifc";
    rev = "1bc301453f674ed21fac4979c65a9a4cb7f2af61";
    hash = "sha256-14ORfbl18UOB6UszBHx7NKxnLdiJxUG7gzrtt0ZriCg=";
  };

  meta = with lib; {
    description = "Configurable fzf completions for fish shell";
    homepage = "https://github.com/gazorby/fifc";
    changelog = "https://github.com/gazorby/fifc/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ montchr ];
    mainProgram = "fifc";
    platforms = platforms.all;
  };
}
