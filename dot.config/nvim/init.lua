-- Little trick to make it so (most) plug install lines can
-- still be copy/pasted in lua
local Plug = vim.fn['plug#']

vim.call('plug#begin')
	-- Official plugin to make configuring the built-in LSP easier
	Plug 'neovim/nvim-lspconfig'
	-- Plugin that makes configuring rust-analyzer easier
	Plug 'simrat39/rust-tools.nvim'

	--Plug 'kyazdani42/nvim-web-devicons'
	Plug('junegunn/fzf', {['do'] = vim.fn['fzf#install']})
	Plug 'junegunn/fzf.vim'

	Plug 'overcache/NeoSolarized'
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'
	Plug 'chriskempson/base16-vim'
	Plug 'kyazdani42/nvim-web-devicons'
	Plug 'onsails/lspkind-nvim'
vim.call('plug#end')

-- Always show the error column so the editor doesn't bounce around
-- when there are warnings/errors in the buffer
vim.opt.signcolumn = "yes"
vim.opt.smarttab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.background = dark
vim.opt.ruler = true
vim.opt.number = true
vim.opt.termguicolors = true

vim.api.nvim_command('colorscheme NeoSolarized')

-- Set up the LSP using rust-analyzer
require('rust-tools').setup({})

-- A few keybindings for common things

-- common options
local opts = { noremap=true, silent=true }

-- go to definition with gj
vim.api.nvim_set_keymap('n', 'gj', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
-- rename with gr
vim.api.nvim_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
-- apply fix with ga
vim.api.nvim_set_keymap('n', 'ga', '<cmd>lua vim.lsp.buf.code_action()<CR><CR>', opts)
-- show documentation with shift-K
vim.api.nvim_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)

vim.api.nvim_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
