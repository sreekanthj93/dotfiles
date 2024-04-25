-- LSP, AutoCompletion and Snippets
return {
  {
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v3.x',
    dependencies = {
      -- LSP Support
      {'neovim/nvim-lspconfig'},             -- Required
      {'williamboman/mason.nvim'},           -- Optional
      {'williamboman/mason-lspconfig.nvim'}, -- Optional

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},         -- Required
      {'hrsh7th/cmp-nvim-lsp'},     -- Required
      {'hrsh7th/cmp-buffer'},       -- Optional
      {'hrsh7th/cmp-path'},         -- Optional
      {'saadparwaiz1/cmp_luasnip'}, -- Optional
      {'hrsh7th/cmp-nvim-lua'},     -- Optional

      -- Snippets
      {'L3MON4D3/LuaSnip'},             -- Required
      {'rafamadriz/friendly-snippets'}, -- Optional

      { 'folke/neodev.nvim', opts = {} },
    },
    event = 'VeryLazy',

    config = function()
      -- IMPORTANT: make sure to setup neodev BEFORE lspconfig
      require('neodev').setup({})

      local lsp_zero = require('lsp-zero')

      lsp_zero.set_sign_icons({
        error = '✘',
        warn = '▲',
        hint = '⚑',
        info = '»'
      })


      lsp_zero.on_attach(function(client, bufnr)
        lsp_zero.default_keymaps({buffer = bufnr})
      end)

      -- LSP Server Settings
      local servers = {
        bashls = {},
        clangd = { filetypes = { 'h', 'c', 'cpp', 'cxx', 'cc' } },
        cmake = {},
        dockerls = {},
        jdtls = {},
        jsonls = {},
        lua_ls = lsp_zero.nvim_lua_ls(),
        pyright = {},
      }

      -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

      -- Set up Mason before anything else
      require('mason').setup({})
      -- Install language servers from mason lsp
      require('mason-lspconfig').setup({
        ensure_installed = vim.tbl_keys(servers),
      })

      -- Setup Lsp Servers
      for ls, ls_opts in pairs(servers) do
        ls_opts.capabilities = capabilities
        require('lspconfig')[ls].setup(ls_opts)
      end

      -- Setup LuaSnip
      local luasnip = require('luasnip')
      require('luasnip.loaders.from_vscode').lazy_load()
      luasnip.config.setup({})

      -- Creates a minimal working config for nvim-cmp
      lsp_zero.extend_cmp()

      -- Completion
      local cmp = require('cmp')
      local cmp_format = lsp_zero.cmp_format()
      local cmp_action = lsp_zero.cmp_action()
      -- local cmp_select_opts = { behavior = cmp.SelectBehavior.Select }

      cmp.setup({
        -- sources for autocompletion
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "buffer" }, -- text within current buffer
          { name = "luasnip" }, -- snippets
          { name = "path" }, -- file system paths
        }),
        preselect = 'item',
        formatting = cmp_format,
        completion = {
          completeopt = 'menu,menuone,noinsert'
        },
        mapping = {
          ['<CR>'] = cmp.mapping.confirm({select = false}),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<Tab>'] = cmp_action.tab_complete(),
          ['<S-Tab>'] = cmp_action.select_prev_or_fallback(),
        }
      })

    end,
  },
}
