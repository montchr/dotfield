[doc: "Sync all sops secrets files with configuration keys"]
update-all-keys:
  fd 'secrets\.(yaml|yml)$' -x sops updatekeys
