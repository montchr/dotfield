{pkgs, ...}: let
  inherit (pkgs) vimPlugins;
in {
  programs.neovim.plugins = [
    # vimPlugins.vim-commentary #      <- "comment stuff out"
    # vimPlugins.vim-dispatch #        <- async build and test dispatcher
    # vimPlugins.vim-dispatch-neovim # <- add neovim support to vim-dispatch
    # vimPlugins.vim-sleuth #          <- "heuristically set buffer options"

    ##: theme
    # vimPlugins.material-nvim

    ##: completions
    # vimPlugins.nvim-cmp
    # vimPlugins.cmp-buffer
    # vimPlugins.cmp-cmdline
    # vimPlugins.cmp-conjure
    # vimPlugins.cmp-nvim-lsp
    # vimPlugins.cmp-path

    ##: editing
    vimPlugins.nvim-autopairs
    # vimPlugins.vim-repeat #     <- "enable repeating supported plugin maps with `.`"
    # vimPlugins.vim-sexp #       <- "precision editing for S-expressions"
    # vimPlugins.vim-sexp-mappings-for-regular-people
    # vimPlugins.vim-surround

    ##: lsp
    vimPlugins.nvim-lspconfig
    vimPlugins.nvim-lsputils

    ##: nav
    vimPlugins.leap-nvim #         <- a clairvoyant interface for on-screen jumps (successor to lightspeed.vim)
    # vimPlugins.vim-argumentative # <- helpers for function args
    # vimPlugins.vim-unimpaired #    <- "pairs of handy bracket mappings"
    vimPlugins.which-key-nvim

    ##: syntax/linting/tree-sitter
    # FIXME: last i checked, ale throws an error on exiting nvim
    # vimPlugins.ale # <- Asynchronous Lint Engine
    # (vimPlugins.nvim-treesitter.withPlugins (_p: pkgs.tree-sitter.allGrammars))
    # NOTE: tree-sitter support is currently experimental!
    # vimPlugins.nvim-treesitter-context # <- "lightweight alternative to context.vim"
    # vimPlugins.nvim-treesitter-textobjects
    # TODO: requires configuration, see repo
    # vimPlugins.nvim-treesitter-refactor

    ##: lang->markdown
    vimPlugins.glow-nvim

    ##: lang->nix
    vimPlugins.vim-nix

    ##: find/filter/preview/pick
    # vimPlugins.popup-nvim
    # vimPlugins.telescope-nvim
    # vimPlugins.vim-abolish # <- "easily search for, substitute, and abbreviate multiple variants of a word"
    #vimPlugins.telescope-fzy-native-nvim
    #vimPlugins.fzf-vim

    ##: vcs
    # vimPlugins.vim-fugitive

    ##: ui
    # vimPlugins.lualine-nvim
    vimPlugins.todo-comments-nvim
    # vimPlugins.undotree
    vimPlugins.vim-css-color
    vimPlugins.vim-gitgutter
    # vimPlugins.vim-vinegar # <- "combine with netrw to create a delicious salad dressing"

    # vimPlugins.null-ls-nvim
    # vimPlugins.tabular
    # vimPlugins.trouble-nvim

    ##: misc
    # vimPlugins.vim-eunuch # <- "helpers for unix"
  ];
}
