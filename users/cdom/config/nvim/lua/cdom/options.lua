vim.opt.fileencoding = "utf-8"

-- [ history ] --

-- Use the system clipboard
vim.opt.clipboard = "unnamedplus"

-- Enable persistent undo
vim.opt.undofile = true

-- Disable backup/swap files
vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.writebackup = false

-- [ completions ] --

vim.opt.completeopt = { "menuone", "noselect" }
vim.opt.shortmess:append("c") -- hide extraneous completion messages

-- [ interface ] --

-- TODO: re-enable when a decent color scheme is enabled
-- vim.opt.cursorline = true -- highlight current line
vim.opt.cmdheight = 1
vim.opt.mouse = "a" -- allow mouse usage
vim.opt.number = true -- display line numbers
vim.opt.numberwidth = 4 -- min number cols for line number gutter
vim.opt.pumheight = 10 -- popup menu height
vim.opt.scrolloff = 4 -- min screen lines above/below of cursor
vim.opt.sidescrolloff = 4 -- min screen cols left/right of cursor
vim.opt.showmode = true
vim.opt.showtabline = 0 -- always show tabs
vim.opt.signcolumn = "yes:1" -- always show sign col to prevent text shift
vim.opt.termguicolors = true
vim.opt.wrap = false

-- [ window management ] --

vim.opt.splitbelow = true
vim.opt.splitright = true

-- [ typography ] --

-- FIXME: consider removal? <https://stackoverflow.com/a/35550626>
-- FIXME: font name format inconsistent across different OS? see: <https://github.com/vim/vim/blob/e4098457ab9c94225b1b0e3c5e06b82b75587971/runtime/doc/gui.txt#L1108-L1122>
-- TODO: name format for Berkeley Mono Variable?
-- vim.opt.guifont = "Iosevka:h15"

-- [ performance ] --

vim.opt.timeoutlen = 1000
vim.opt.updatetime = 300

-- Avoid redrawing the screen when executing macros
vim.opt.lazyredraw = true

-- [ formatting ] --

-- Prefer space over tab indentation
vim.opt.smartindent = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2
vim.opt.linebreak = true

-- [ navigation ] --

vim.opt.whichwrap:append("<,>,[,],h,l") -- keys allowed to move to prev/next line

-- [ search ] --

vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Global substitution by default
vim.opt.gdefault = true

-- Use ripgrep for builtin grepping
vim.opt.grepprg = "rg --vimgrep"
-- TODO: vim.opt.grepformat "%f:%l:%c:%m"

-- Highlight search results
vim.opt.hlsearch = true

-- Fuzzy file search patterns
vim.opt.path = { ".", "**" }
