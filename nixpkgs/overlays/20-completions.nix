self: super: {
  darwin-zsh-completions = super.runCommandNoCC "darwin-zsh-completion-0.0.0"
    { preferLocalBuild = true; }
      ''
        mkdir -p $out/share/zsh/site-functions
        cat <<-'EOF' > $out/share/zsh/site-functions/_darwin-rebuild
        #compdef darwin-rebuild
        #autoload
        _nix-common-options
        local -a _1st_arguments
        _1st_arguments=(
          'switch:Build, activate, and update the current generation'\
          'build:Build without activating or updating the current generation'\
          'check:Build and run the activation sanity checks'\
          'changelog:Show most recent entries in the changelog'\
        )
        _arguments \
          '--list-generations[Print a list of all generations in the active profile]'\
          '--rollback[Roll back to the previous configuration]'\
          {--switch-generation,-G}'[Activate specified generation]'\
          '(--profile-name -p)'{--profile-name,-p}'[Profile to use to track current and previous system configurations]:Profile:_nix_profiles'\
          '1:: :->subcmds' && return 0
        case $state in
          subcmds)
            _describe -t commands 'darwin-rebuild subcommands' _1st_arguments
          ;;
        esac
        EOF
      '';
}
