local Util = require('util')
local nmap = Util.nmap
local keymap = Util.keymap

-- Remap space as leader key
keymap('', '<Space>', '<Nop>')
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Better window navigation
nmap('<C-h>', '<C-w>h') -- left window
nmap('<C-k>', '<C-w>k') -- up window
nmap('<C-j>', '<C-w>j') -- down window
nmap('<C-l>', '<C-w>l') -- right window

-- Resize with arrows when using multiple windows
nmap('<C-Up>', ':resize -2<CR>')
nmap('<C-Down>', ':resize +2<cr>')
nmap('<C-Right>', ':vertical resize -2<cr>')
nmap('<C-Left>', ':vertical resize +2<cr>')

-- Clear search highlight
-- nmap('<leader>h', ':nohlsearch<CR>')
nmap('<leader>h', ':nohlsearch<CR>')

-- navigate tabs
nmap('<tab>', ':tabnext<cr>') -- Next Tab
nmap('<s-tab>', ':tabprevious<cr>') -- Previous tab

-- insert --
-- press jk fast to exit insert mode
keymap('i', 'jk', '<esc>') -- Insert mode -> jk -> Normal mode
keymap('i', 'kj', '<esc>') -- Insert mode -> kj -> Normal mode

-- Diagnostic keymaps
nmap('[d', vim.diagnostic.goto_prev, 'Go to previous diagnostic message')
nmap(']d', vim.diagnostic.goto_next, 'Go to next diagnostic message')
nmap('<leader>de', vim.diagnostic.open_float, 'Open floating diagnostic message')
nmap('<leader>dl', vim.diagnostic.setloclist, 'Open diagnostics list')
