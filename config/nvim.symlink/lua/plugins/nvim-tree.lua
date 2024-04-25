-- Nvimtree (File Explorer)
return {
  'nvim-tree/nvim-tree.lua',
  lazy = false,
  dependencies = {
    'nvim-tree/nvim-web-devicons',
  },
  keys = {
    { '<leader>e', '<cmd>NvimTreeToggle<cr>', desc = 'Toggle NvimTree' },
  },
  config = function()
    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1

    require('nvim-tree').setup({
      disable_netrw = true,
      hijack_netrw = true,
      hijack_cursor = true,
      sync_root_with_cwd = true,
      sort_by = 'case_sensitive',
      view = {
        width = 40,
        side = 'left',
      },
      filters = {
        dotfiles = true,
      },
      on_attach = function(bufnr)
        local api = require('nvim-tree.api')

        local function opts(desc)
          return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
        end
        -- default mappings
        api.config.mappings.default_on_attach(bufnr)
        -- custom mappings
        vim.keymap.set('n', '<C-t>', api.tree.change_root_to_parent,        opts('Up'))
        vim.keymap.set('n', '?',     api.tree.toggle_help,                  opts('Help'))
      end,
    })
  end
}
