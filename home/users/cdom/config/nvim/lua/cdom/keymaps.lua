local keymap = vim.keymap.set
local opts = { silent = true }

-- <Space> as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

-- [ NORMAL ] ------------------------------------------------------------------

-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Resize with arrow keys
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical :resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical :resize +2<CR>", opts)

-- Buffer navigation
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)

-- [ INSERT ] ------------------------------------------------------------------

-- Quickly press `jk` to enter INSERT mode
keymap("i", "jk", "<ESC>", opts)
