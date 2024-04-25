-- Ui related stuff
return {
  -- Set lualine as statusline
  {
    'nvim-lualine/lualine.nvim',
    event = 'VeryLazy',
    -- See `:help lualine.txt`
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    opts = {
      options = {
        icons_enabled = true,
        theme = 'tokyonight',
        -- component_separators = '|',
        -- section_separators = '',
      },
    },
  },

  {
    -- Add indentation guides even on blank lines
    'lukas-reineke/indent-blankline.nvim',
    event = { 'BufReadPost', 'BufNewFile' },
    opts = {
      char = '┊',
      filetype_exclude = {
        'alpha',
        'dashboard',
        'help',
        'lazy',
        'lazy',
        'lazyterm',
        'lspinfo',
        'mason',
        'neo-tree',
        'notify',
        'nvim-tree',
        'TelescopePrompt',
        'TelescopeResults',
        'terminal',
        'toggleterm',
        'Trouble',
      },
      buftype_exclude = { 'terminal' },
      show_current_context = true,
      show_current_context_start = true,
      show_trailing_blankline_indent = true,
    },
  },

  -- better vim.ui
  {
    'stevearc/dressing.nvim',
    lazy = true,
    init = function()
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.select = function(...)
        require('lazy').load({ plugins = { 'dressing.nvim' } })
        return vim.ui.select(...)
      end
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.input = function(...)
        require('lazy').load({ plugins = { 'dressing.nvim' } })
        return vim.ui.input(...)
      end
    end,
  },

  {
    'folke/noice.nvim',
    event = 'VeryLazy',
    dependencies = 'MunifTanjim/nui.nvim',
    opts = {
      lsp = {
        override = {
          ['vim.lsp.util.convert_input_to_markdown_lines'] = true,
          ['vim.lsp.util.stylize_markdown'] = true,
          ['cmp.entry.get_documentation'] = true,
        },
      },
      routes = {
        {
          filter = {
            event = 'msg_show',
            any = {
              { find = '%d+L, %d+B' },
              { find = '; after #%d+' },
              { find = '; before #%d+' },
            },
          },
          view = 'mini',
        },
      },
      presets = {
        bottom_search = true,
        command_palette = true,
        long_message_to_split = true,
        inc_rename = true,
      },
    },
    -- stylua: ignore
    keys = {
      { '<S-Enter>', function() require('noice').redirect(vim.fn.getcmdline()) end, mode = 'c', desc = 'Redirect Cmdline' },
      { '<leader>snl', function() require('noice').cmd('last') end, desc = 'Noice Last Message' },
      { '<leader>snh', function() require('noice').cmd('history') end, desc = 'Noice History' },
      { '<leader>sna', function() require('noice').cmd('all') end, desc = 'Noice All' },
      { '<leader>snd', function() require('noice').cmd('dismiss') end, desc = 'Dismiss All' },
      { '<c-f>', function() if not require('noice.lsp').scroll(4) then return '<c-f>' end end, silent = true, expr = true, desc = 'Scroll forward', mode = {'i', 'n', 's'} },
      { '<c-b>', function() if not require('noice.lsp').scroll(-4) then return '<c-b>' end end, silent = true, expr = true, desc = 'Scroll backward', mode = {'i', 'n', 's'}},
    },
  },

  -- See current buffers at the top of the editor
  {
    'akinsho/bufferline.nvim',
    version = 'v3.*',
    dependencies = 'nvim-tree/nvim-web-devicons',
    event = 'VeryLazy',
    keys = {
      { '<leader>bp', '<Cmd>BufferLineTogglePin<CR>', desc = 'Toggle pin' },
      { '<leader>bP', '<Cmd>BufferLineGroupClose ungrouped<CR>', desc = 'Delete non-pinned buffers' },
    },
    opts = {
      options = {
        separator_style = 'slant',
        numbers = function(opts)
          if vim.api.nvim_get_current_buf() == opts.id then return '' end
          return opts.ordinal
        end,
        offsets = {
          {
            filetype = 'NvimTree',
            text = 'Nvim Tree',
            highlight = 'Directory',
            text_align = 'center',
          },
        },
      }
    },
  },
}
