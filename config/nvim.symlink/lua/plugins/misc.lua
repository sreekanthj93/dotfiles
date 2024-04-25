-- Miscelaneous
local nmap = require('util').nmap

return {
  -- Comment
  {
    'numToStr/Comment.nvim',
    event = 'VeryLazy',
    opts = {},
    config = function()
      require('Comment').setup()

      local api = require('Comment.api')
      -- Toggle current line (linewise) using C-/
      nmap('<C-/>', api.toggle.linewise.current, 'Comment toggle linewise')
      -- Toggle current line (blockwise) using C-\
      nmap('<C-\\>', api.toggle.blockwise.current, 'Comment toggle blockwise')
    end
  },

  -- Detect tabstop and shiftwidth automatically
  { 'tpope/vim-sleuth', lazy = false },

  -- Surround stuff with the ys-, cs-, ds- commands
  { 'tpope/vim-surround', lazy = false },

  {
    'notjedi/nvim-rooter.lua',
    lazy = false,
    config = function()
      require('nvim-rooter').setup()
    end
  },

  -- Useful plugin to show you pending keybinds.
  {
    'folke/which-key.nvim',
    event = 'VeryLazy',
    opts = {},
    config = function()
      local wk = require('which-key')

      local setup = {
        popup_mappings = {
          scroll_down = '<c-f>', -- binding to scroll down inside the popup
          scroll_up = '<c-b>', -- binding to scroll up inside the popup
        },
        triggers = 'auto',
        triggers_blacklist = {
          -- list of mode / prefixes that should never be hooked by WhichKey
          -- this is mostly relevant for key maps that start with a native binding
          -- most people should not need to change this
          i = { 'j', 'k' },
          v = { 'j', 'k' },
        },
      }

      local mappings = {
        ['<leader>'] = {
          k = { '<cmd>bdelete<cr>', 'Kill Buffer' }, -- Close current file
          q = { '<cmd>qa<cr>', 'Quit' }, -- Quit Neovim after saving the file
          w = { '<cmd>w!<cr>', 'Save' }, -- Save current file
        },
      }

      wk.setup(setup)
      wk.register(mappings, {})
    end
  },
}
