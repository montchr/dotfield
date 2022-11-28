vim.opt.fileencoding = "utf-8"

-- [ history ] --

vim.opt.clipboard = "unnamedplus"               -- use the system clipboard
vim.opt.undofile = true -- enable persistent undo

-- [ completions ] --

vim.opt.completeopt = { "menuone", "noselect" }
vim.opt.shortmess:append "c" -- hide extraneous completion messages

-- [ interface ] --

-- TODO: re-enable when a decent color scheme is enabled
-- vim.opt.cursorline = true -- highlight current line
vim.opt.cmdheight = 1                            
vim.opt.mouse = "a" -- allow mouse usage
vim.opt.number = true -- display line numbers
vim.opt.numberwidth = 4 -- min number cols for line number gutter
vim.opt.pumheight = 10-- popup menu height
vim.opt.scrolloff = 8 -- min screen lines above/below of cursor
vim.opt.sidescrolloff = 8 -- min screen cols left/right of cursor
vim.opt.showmode = true
vim.opt.showtabline = 0 -- always show tabs
-- TODO: re-enable when a decent color scheme is enabled
-- vim.opt.signcolumn = "yes" -- always show sign col to prevent text shift
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.termguicolors = true
vim.opt.wrap = false

-- [ typography ] --

-- vim.opt.guifont = "monospace:h17"

-- [ performance ] --

vim.opt.timeoutlen = 1000 -- timeout for mapped sequence to finish
vim.opt.updatetime = 300 -- faster completion

-- [ rendering ] --

-- vim.opt.conceallevel = 0                      -- make '``' visible in markdown files

-- [ integrity ] --

vim.opt.backup = false                           -- create a backup file
vim.opt.swapfile = true
vim.opt.writebackup = true

-- [ formatting ] --

vim.opt.smartcase = true
vim.opt.smartindent = true
vim.opt.expandtab = true -- convert tabs to spaces
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.linebreak = true

-- [ navigation ] --

vim.opt.whichwrap:append("<,>,[,],h,l") -- keys allowed to move to prev/next line

-- [ search ] --

vim.opt.hlsearch = true    -- highlight search results
vim.opt.ignorecase = true  -- ignore case in search patterns
