# FIXME: use or lose (not used currently)
{
  programs.vscode.userSettings = {
    "[javascript]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "[json]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "[jsonc]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "[markdown]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "[typescript]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "[typescriptreact]" = {
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
    };
    "colorize.languages" = [
      "javascript"
      "css"
      "sass"
      "scss"
      "less"
      "postcss"
      "sss"
      "stylus"
      "xml"
      "svg"
    ];
    "css.format.spaceAroundSelectorSeparator" = true;
    "debug.console.fontFamily" = "'Iosevka Nerd Font Mono'";
    "debug.javascript.codelens.npmScripts" = "never";
    "diffEditor.renderSideBySide" = false;
    "editor.codeLensFontFamily" = "'Iosevka Nerd Font Mono'";
    "editor.cursorSmoothCaretAnimation" = true;
    "editor.definitionLinkOpensInPeek" = true;
    "editor.fontFamily" = "'Iosevka Nerd Font', 'Fira Code', 'JetBrains Mono', Menlo, Monaco, 'Courier New', monospace";
    "editor.fontLigatures" = "'calt' off, 'dlig' on";
    "editor.fontSize" = 15;
    "editor.fontWeight" = "normal";
    "editor.formatOnSave" = true;
    "editor.inlayHints.fontFamily" = "'Iosevka Nerd Font Mono'";
    "editor.linkedEditing" = true;
    "editor.minimap.autohide" = true;
    "editor.minimap.enabled" = false;
    "editor.occurrencesHighlight" = false;
    "editor.parameterHints.cycle" = true;
    "editor.quickSuggestions" = {
      strings = "on";
    };
    "editor.renderLineHighlightOnlyWhenFocus" = true;
    "editor.scrollBeyondLastLine" = false;
    "editor.tabSize" = 2;
    "errorLens.enabledInMergeConflict" = false;
    "errorLens.fontFamily" = "'Iosevka Nerd Font Mono'";
    "errorLens.fontStyleItalic" = true;
    "errorLens.gutterIconsEnabled" = true;
    "errorLens.messageTemplate" = "$message [$source:$code]";
    "eslint.alwaysShowStatus" = true;
    "eslint.packageManager" = "yarn";
    "eslint.run" = "onSave";
    "evenBetterToml.formatter.columnWidth" = 80;
    "explorer.confirmDelete" = false;
    "explorer.confirmDragAndDrop" = false;
    "explorer.excludeGitIgnore" = true;
    "extensions.closeExtensionDetailsOnViewChange" = true;
    "files.associations" = {
      dep5 = "debian-copyright";
    };
    "files.autoGuessEncoding" = true;
    "files.insertFinalNewline" = true;
    "git.autoStash" = true;
    "git.closeDiffOnOperation" = true;
    "git.defaultCloneDirectory" = "~/Projects/scratch";
    "git.enableCommitSigning" = true;
    "git.fetchOnPull" = true;
    "git.repositoryScanMaxDepth" = 2;
    "git.scanRepositories" = [
      "~/Projects/work"
      "~/Projects/sources"
      "~/Projects/contrib"
    ];
    "git.smartCommitChanges" = "tracked";
    "git.suggestSmartCommit" = false;
    "github.gitProtocol" = "ssh";
    "gitlens.blame.ignoreWhitespace" = true;
    "gitlens.codeLens.enabled" = false;
    "gitlens.currentLine.enabled" = false;
    "gitlens.currentLine.uncommittedChangesFormat" = "✏️ \${ago}";
    "gitlens.hovers.currentLine.over" = "line";
    "gitlens.hovers.enabled" = false;
    "gitlens.showWelcomeOnInstall" = false;
    "hoverlens.debounceUpdate" = 1000;
    "javascript.updateImportsOnFileMove.enabled" = "always";
    "magit.forge-enabled" = true;
    "markdown.experimental.editor.pasteLinks.enabled" = false;
    "markdown.preview.fontFamily" = "'IBM Plex Sans', -apple-system, BlinkMacSystemFont, 'Segoe WPC', 'Segoe UI', system-ui, 'Ubuntu', 'Droid Sans', sans-serif";
    "markdown.preview.typographer" = true;
    "material-icon-theme.saturation" = 0.5;
    "nix.enableLanguageServer" = true;
    "nix.serverPath" = "/etc/profiles/per-user/cdom/bin/nil";
    "prettier.requireConfig" = true;
    "problems.showCurrentInStatus" = true;
    "redhat.telemetry.enabled" = false;
    "rewrap.autoWrap.enabled" = true;
    "scm.diffDecorations" = "gutter";
    "scm.inputFontFamily" = "editor";
    "search.exclude" = {
      "**/vendor" = true;
    };
    "search.smartCase" = true;
    "search.useGlobalIgnoreFiles" = true;
    "tailwindCSS.emmetCompletions" = true;
    "terminal.external.osxExec" = "~/Applications/kitty.app";
    "terminal.integrated.cursorBlinking" = true;
    "terminal.integrated.cursorStyle" = "line";
    "terminal.integrated.fontFamily" = "'Iosevka Nerd Font Mono'";
    "typescript.updateImportsOnFileMove.enabled" = "always";
    "vim.cursorStylePerMode.insert" = "line";
    "vim.cursorStylePerMode.normal" = "block";
    "vim.cursorStylePerMode.replace" = "underline";
    "vim.gdefault" = true;
    "vim.handleKeys" = {
      "<C-a>" = false;
      "<C-d>" = true;
      "<C-f>" = true;
      "<C-s>" = false;
      "<C-z>" = false;
    };
    "vim.highlightedyank.enable" = true;
    "vim.history" = 300;
    "vim.hlsearch" = true;
    "vim.leader" = "<space>";
    "vim.mouseSelectionGoesIntoVisualMode" = false;
    "vim.showMarksInGutter" = true;
    "vim.smartRelativeLine" = true;
    "vim.sneak" = true;
    "vim.sneakReplacesF" = true;
    "vim.useSystemClipboard" = true;
    "vim.visualstar" = true;
    "vscode-neovim.mouseSelectionStartVisualMode" = true;
    "vsicons.dontShowNewVersionMessage" = true;
    "window.autoDetectColorScheme" = true;
    "window.dialogStyle" = "custom";
    "window.restoreFullscreen" = true;
    "workbench.activityBar.visible" = false;
    "workbench.colorTheme" = "Vitesse Dark";
    "workbench.editor.enablePreview" = false;
    "workbench.editor.enablePreviewFromCodeNavigation" = true;
    "workbench.editor.enablePreviewFromQuickOpen" = true;
    "workbench.editor.limit.enabled" = true;
    "workbench.editor.limit.excludeDirty" = true;
    "workbench.editor.limit.perEditorGroup" = true;
    "workbench.editor.pinnedTabSizing" = "shrink";
    "workbench.editor.preferHistoryBasedLanguageDetection" = true;
    "workbench.fontAliasing" = "auto";
    "workbench.iconTheme" = "material-icon-theme";
    "workbench.layoutControl.enabled" = false;
    "workbench.preferredDarkColorTheme" = "Vitesse Dark";
    "workbench.preferredHighContrastColorTheme" = "GitHub Dark High Contrast";
    "workbench.preferredHighContrastLightColorTheme" = "GitHub Light High Contrast";
    "workbench.preferredLightColorTheme" = "GitHub Light Monochrome";
    "zenMode.fullScreen" = false;
  };
}
