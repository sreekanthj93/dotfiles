-- vim Options

local opt = vim.opt

-- opt.autowrite = true -- Enable auto write
opt.clipboard = 'unnamedplus' -- Sync with system clipboard
opt.completeopt = 'menu,menuone,noselect'
opt.conceallevel = 3 -- Hide * markup for bold and italic
opt.confirm = true -- Confirm to save changes before exiting modified buffer
-- opt.formatoptions = 'jcroqlnt' -- tcqj
-- opt.grepformat = '%f:%l:%c:%m'
-- opt.grepprg = 'rg --vimgrep'
opt.inccommand = 'nosplit' -- preview incremental substitute
opt.laststatus = 0
opt.list = true -- Show some invisible characters (tabs...
opt.mouse = 'a' -- Enable mouse mode
opt.number = true -- Print line number
opt.pumblend = 10 -- Popup blend
opt.pumheight = 10 -- Maximum number of entries in a popup
opt.relativenumber = true -- Relative line numbers
opt.scrolloff = 4 -- Lines of context
opt.sessionoptions = { 'buffers', 'curdir', 'tabpages', 'winsize' }
opt.shortmess:append { W = true, I = true, c = true }
opt.showmode = false -- Dont show mode since we have a statusline
opt.sidescrolloff = 8 -- Columns of context
opt.signcolumn = 'yes' -- Always show the signcolumn, otherwise it would shift the text each time
opt.spelllang = { 'en' }
opt.splitbelow = true -- Put new windows below current
opt.splitright = true -- Put new windows right of current
opt.termguicolors = true -- True color support
opt.timeoutlen = 500 -- speed must be under 500ms inorder for keys to work, increase if you are not able to.
opt.undofile = true
opt.undolevels = 1000
opt.updatetime = 250 -- Save swap file and trigger CursorHold
opt.wildmode = 'longest:full,full' -- Command-line completion mode
opt.winminwidth = 5 -- Minimum window width
opt.wrap = false -- Disable line wrap

-- Searching
opt.hlsearch = true
opt.ignorecase = true -- Ignore case
opt.incsearch = true
opt.smartcase = true -- Don't ignore case with capitals
opt.tabstop = 4 -- Number of spaces tabs count for

-- Indentation
opt.autoindent = true
opt.expandtab = true -- Use spaces instead of tabs
opt.shiftround = true -- Round indent
opt.shiftwidth = 4 -- Size of an indent
opt.smartindent = true -- Insert indents automatically

vim.opt.listchars = {
  -- space = '·',
  -- eol = '↲',
  nbsp = '␣',
  trail = '·',
  precedes = '❮',
  extends = '❯',
  tab = '→ ',
  conceal = '※',
}
vim.opt.list = true

-- Cursor
opt.cursorline = true -- Enable highlighting of the current line
