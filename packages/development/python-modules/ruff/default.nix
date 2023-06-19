{
  lib,
  python3,
  fetchPypi,
  rustPlatform,
}:
python3.pkgs.buildPythonApplication rec {
  pname = "ruff";
  version = "0.0.272";
  format = "pyproject";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-JzoB3Iw8T9TCr36npnyNObsJvORm5kDdFwA02nXRTKs=";
  };

  nativeBuildInputs = [
    rustPlatform.maturinBuildHook
  ];

  pythonImportsCheck = ["ruff"];

  meta = with lib; {
    description = "An extremely fast Python linter, written in Rust";
    homepage = "https://pypi.org/project/ruff/";
    license = licenses.mit;
    maintainers = with maintainers; [];
  };
}
