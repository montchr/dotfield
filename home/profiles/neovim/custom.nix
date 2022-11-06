##: Sources
# - https://github.com/Olical/magic-kit
# - https://github.com/teto/home
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.xdg) configHome;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  l = lib // builtins;
  configBasePath = "${configHome}/dotfield/home/users/${config.home.username}/config";
  cfg = config.programs.neovim;

  luaPlugin = attrs:
    attrs
    // {
      type = "lua";
      config = l.optionalString (attrs ? config && attrs.config != null) (luaBlock attrs.plugin.pname attrs.config);
    };
  luaPlugin' = plugin: config: luaPlugin {inherit plugin config;};

  luaBlock = title: content: ''
    --: ${title} {{{
    ${content}
    --: }}}
  '';

  muPlugins = with pkgs.vimPlugins; [
    # packer compat with nix-provided plugins
    # TODO: verify whether this actually helps or is unnecessary
    #(luaPlugin' packer-nvim ''
    #  require('packer').init({
    #    require_dependencies = false
    #  })
    #'')

    ##: fennel
    aniseed
    conjure
    # FIXME: not in nixpkgs -- necessary?
    # https://github.com/Olical/magic-kit/blob/main/fnl/magic/init.fnl#L44
    # nvim-local-fennel

    ##: performance
    impatient-nvim

    vim-commentary #      <- "comment stuff out"
    vim-dispatch #        <- async build and test dispatcher
    vim-dispatch-neovim # <- add neovim support to vim-dispatch
    vim-sleuth #          <- "heuristically set buffer options"
  ];

  themePlugins = with pkgs.vimPlugins; [
    material-nvim
  ];
in {
  xdg.configFile = {
    "nvim".source = mkOutOfStoreSymlink "${configBasePath}/nvim";

    # "nvim/fnl" = {
    #   source = ./fnl;
    #   onChange = ''
    #     rm -rf "${configHome}/nvim/lua"
    #     ${l.getExe cfg.package} --headless \
    #       -c "lua require('aniseed.env').init()" \
    #       -c q
    #   '';
    # };
  };

  programs.neovim = {
    plugins = with pkgs.vimPlugins;
      muPlugins
      # TODO
      # ++ themePlugins
      ++ [
        ##: completions
        nvim-cmp
        cmp-buffer
        cmp-cmdline
        cmp-conjure
        cmp-nvim-lsp
        cmp-path

        ##: formatting
        # FIXME: is this for syntax support? vim-sleuth should handle the actual tooling support
        # editorconfig-vim

        ##: editing
        vim-repeat #     <- "enable repeating supported plugin maps with `.`"
        vim-sexp #       <- "precision editing for S-expressions"
        vim-sexp-mappings-for-regular-people
        vim-surround
        nvim-autopairs

        ##: lsp
        nvim-lspconfig
        nvim-lsputils

        ##: nav
        leap-nvim #         <- a clairvoyant interface for on-screen jumps (successor to lightspeed.vim)
        vim-argumentative # <- helpers for function args
        vim-unimpaired #    <- "pairs of handy bracket mappings"
        which-key-nvim

        ##: syntax/linting/tree-sitter
        ale # <- Asynchronous Lint Engine
        (nvim-treesitter.withPlugins (_p: pkgs.tree-sitter.allGrammars))
        # NOTE: tree-sitter support is currently experimental!
        nvim-treesitter-context # <- "lightweight alternative to context.vim"
        nvim-treesitter-textobjects
        # TODO: requires configuration, see repo
        # nvim-treesitter-refactor

        ##: lang support
        vim-fish
        vim-nix

        ##: find/filter/preview/pick
        plenary-nvim
        popup-nvim
        telescope-nvim
        vim-abolish # <- "easily search for, substitute, and abbreviate multiple variants of a word"
        #telescope-fzy-native-nvim
        #fzf-vim

        ##: vcs
        vim-fugitive

        ##: ui
        lualine-nvim
        todo-comments-nvim
        undotree
        vim-css-color
        vim-gitgutter
        vim-vinegar # <- "combine with netrw to create a delicious salad dressing"

        null-ls-nvim
        tabular
        trouble-nvim

        ##: misc
        vim-eunuch # <- "helpers for unix"
      ];
  };
}
