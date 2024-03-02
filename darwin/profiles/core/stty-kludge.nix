{lib, ...}: {
  environment.systemPath = lib.mkBefore ["$HOME/.local/bin"];

  # Link native /bin/stty to user bin directory
  # In some rare cases, GNU coreutils stty causes issues on Darwin.
  #
  # Requires that $HOME/.local/bin be added to user's $PATH.
  #
  # <https://github.com/akinomyoga/ble.sh/issues/63#issuecomment-715305422>
  system.activationScripts.postUserActivation.text = ''
    userBin="$HOME/.local/bin"
    target="''${userBin}/stty"

    if [[ ! -d "$userBin" ]]; then
      $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$userBin"
    fi

    if [[ ! -h "$target" ]]; then
      $DRY_RUN_CMD ln -sf $VERBOSE_ARG /bin/stty "$target"
    fi
  '';
}
