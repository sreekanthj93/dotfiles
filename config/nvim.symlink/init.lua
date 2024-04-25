-- Enable loader
local ver = vim.version()
if vim.version.gt(ver, {0, 9}) then
  vim.loader.enable()
end

require('core.keymaps') -- Added this line to the initial file
require('core.options')
require('core.lazy')
require('core.colorscheme')
require('core.autocmds')
