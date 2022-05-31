channels: final: prev: {
  __dontExport = true;

  # Handled by the Homebrew module
  # This populates a dummy package to satisfy the requirement
  firefox-dotfield = (final.runCommand "firefox-0.0.0" {} "mkdir $out");
}
