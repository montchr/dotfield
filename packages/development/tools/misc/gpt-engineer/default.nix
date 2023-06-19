{
  lib,
  python3,
  fetchFromGitHub,
  ruff,
  black,
}:
python3.pkgs.buildPythonApplication rec {
  pname = "gpt-engineer";
  version = "0.0.3";
  format = "pyproject";

  src = fetchFromGitHub {
    owner = "AntonOsika";
    repo = "gpt-engineer";
    rev = "v${version}";
    hash = "sha256-pRUjU6cpoOtNj5IIBrZ+jvwrmSBCmZUEsoujNDfiTfA=";
  };

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = [
    black
    python3.pkgs.openai
    python3.pkgs.typer
    ruff
  ];

  pythonImportsCheck = ["gpt-engineer"];

  meta = with lib; {
    description = "Specify what you want it to build, the AI asks for clarification, and then builds it";
    homepage = "https://github.com/AntonOsika/gpt-engineer";
    license = licenses.mit;
    maintainers = with maintainers; [montchr];
  };
}
