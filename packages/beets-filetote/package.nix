# FIXME: tries to write config dir during tests! build fails
# Source: <https://github.com/Ramblurr/nixcfg/blob/5140a2049ac6dfae528ca60c4ffccbff553d638d/pkgs/beets-filetote.nix>
{
  lib,
  python3Packages,
  fetchFromGitHub,
  beets,
}:

python3Packages.buildPythonApplication rec {
  pname = "beets-filetote";
  version = "0.4.9";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "gtronset";
    repo = "beets-filetote";
    rev = "v${version}";
    hash = "sha256-pZ6c2XQMSiiPHyZMLSiSE+LXeCfi3HEWtsTK5DP9YZE=";
  };

  build-system = [
    python3Packages.poetry-core
  ];

  postPatch = ''
    sed -i -e '/audible/d' tests/helper.py
  '';

  pytestFlagsArray = [ "-r fEs" ];

  disabledTests = [
    "test_audible_m4b_files.py"

    # XXX: Needs update for Beets v2.0.0
    # <https://github.com/gtronset/beets-filetote/discussions/160>
    "test_move_on_modify_command"
    "test_prune_modify_query"
  ];

  nativeCheckInputs = with python3Packages; [
    pytestCheckHook
    beets
    toml
  ];

  preBuild = ''
    export HOME=$(mktemp -d)
  '';

  pythonImportsCheck = [
    "beetsplug.filetote"
    "beetsplug.filetote_dataclasses"
  ];

  meta = with lib; {
    description = "Beets plugin to move non-music files during the import process";
    homepage = "https://github.com/gtronset/beets-filetote";
    changelog = "https://github.com/gtronset/beets-filetote/blob/${src.rev}/CHANGELOG.md";
    maintainers = with maintainers; [
      dansbandit
      montchr
    ];
    license = licenses.mit;
    inherit (beets.meta) platforms;
  };
}
