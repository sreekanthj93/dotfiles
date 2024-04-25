local Util = require('util')

-- this will return a function that calls telescope.
-- cwd will default to lazyvim.util.get_root
-- for `files`, git_files or find_files will be chosen depending on .git
local function telescope_fn(builtin, opts)
  local params = { builtin = builtin, opts = opts }
  return function()
    builtin = params.builtin
    opts = params.opts
    opts = vim.tbl_deep_extend('force', { cwd = Util.get_root() }, opts or {})
    if builtin == 'files' then
      if vim.loop.fs_stat((opts.cwd or vim.loop.cwd()) .. '/.git') then
        opts.show_untracked = true
        builtin = 'git_files'
      else
        builtin = 'find_files'
      end
    end
    if opts.cwd and opts.cwd ~= vim.loop.cwd() then
      opts.attach_mappings = function(_, map)
        map('i', '<a-c>', function()
          local action_state = require('telescope.actions.state')
          local line = action_state.get_current_line()
          telescope_fn(
            params.builtin,
            vim.tbl_deep_extend('force', {}, params.opts or {}, { cwd = false, default_text = line })
          )()
        end)
        return true
      end
    end

    require('telescope.builtin')[builtin](opts)
  end
end

-- Telescope (Fuzzy Finder)
return {
  'nvim-telescope/telescope.nvim',
  branch = '0.1.x',
  dependencies = {
    'nvim-lua/plenary.nvim',
    {
      -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'make',
      cond = vim.fn.executable('make') == 1
    },
    -- 'desdic/telescope-rooter.nvim'
  },
  cmd = 'Telescope',
  lazy = true,
  config = function()
    require('telescope').setup({})

    -- Enable telescope fzf native, if installed
    pcall(require('telescope').load_extension, 'fzf')

    -- Enable rooter
    -- pcall(require('telescope').load_extension, 'rooter')
  end,

  keys = {
    { '<C-p>', telescope_fn('files'), desc = 'Find Files' },
    -- find
    { '<leader>fb', '<cmd>Telescope buffers<cr>', desc = 'Find Buffers' },
    { '<leader>ff', telescope_fn('files'), desc = 'Find Files (root dir)' },
    { '<leader>fF', telescope_fn('files', { cwd = false }), desc = 'Find Files (cwd)' },
    { '<leader>fr', '<cmd>Telescope oldfiles<cr>', desc = 'Recent' },
    { '<leader>fR', telescope_fn('oldfiles', { cwd = vim.loop.cwd() }), desc = 'Recent (cwd)' },

    -- git
    { '<leader>gf', '<cmd>Telescope git_files<cr>', desc = 'Find Files (root dir)' },
    { '<leader>gc', '<cmd>Telescope git_commits<cr>', desc = 'commits' },
    { '<leader>gs', '<cmd>Telescope git_status<cr>', desc = 'status' },

    -- search
    { '<leader>/', telescope_fn('live_grep'), desc = 'Grep (root dir)' },
    { '<leader>sw', '<cmd>Telescope grep_string<cr>', desc = '[S]earch current [W]ord' },
    { '<leader>sg', telescope_fn('live_grep'), desc = '[S]earch by [G]rep' },

    -- symbols
    { '<leader>ss',
      telescope_fn('lsp_document_symbols', {
        symbols = {
          'Class',
          'Function',
          'Method',
          'Constructor',
          'Interface',
          'Module',
          'Struct',
          'Trait',
          'Field',
          'Property',
        },
      }),
      desc = 'Goto Symbol (Workspace)'
    },
    { '<leader>sS',
      telescope_fn('lsp_dynamic_workspace_symbols', {
        symbols = {
          'Class',
          'Function',
          'Method',
          'Constructor',
          'Interface',
          'Module',
          'Struct',
          'Trait',
          'Field',
          'Property',
        },
      }),
      desc = 'Goto Symbol (Workspace)'
    },
  },
}
