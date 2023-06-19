{
  lib,
  python3,
  fetchFromGitHub,
}:
python3.pkgs.buildPythonApplication rec {
  pname = "black";
  version = "23.3.0";
  format = "pyproject";

  src = fetchFromGitHub {
    owner = "psf";
    repo = "black";
    rev = version;
    hash = "sha256-qaoeg7+PgoTURoDwrRaBtv4QR8Ea2cp+pkphWM+1uoA=";
  };

  nativeBuildInputs = [
    python3.pkgs.hatch-fancy-pypi-readme
    python3.pkgs.hatch-vcs
    python3.pkgs.hatchling
  ];

  propagatedBuildInputs = with python3.pkgs; [
    click
    mypy-extensions
    packaging
    pathspec
    platformdirs
    tomli
    typed-ast
    typing-extensions
  ];

  passthru.optional-dependencies = with python3.pkgs; {
    colorama = [
      colorama
    ];
    d = [
      aiohttp
    ];
    jupyter = [
      ipython
      tokenize-rt
    ];
    uvloop = [
      uvloop
    ];
  };

  pythonImportsCheck = ["black"];

  meta = with lib; {
    description = "The uncompromising Python code formatter";
    homepage = "https://github.com/psf/black";
    changelog = "https://github.com/psf/black/blob/${src.rev}/CHANGES.md";
    license = licenses.mit;
    maintainers = with maintainers; [];
  };
}
