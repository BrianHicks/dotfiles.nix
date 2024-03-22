local util = require "formatter.util"

require("formatter").setup {
  logging = true,
  log_level = vim.log.levels.WARN,
  filetype = {
    -- Formatter configurations for filetype "lua" go here
    -- and will be executed in order
    -- lua = {
    --   -- "formatter.filetypes.lua" defines default configurations for the
    --   -- "lua" filetype
    --   require("formatter.filetypes.lua").stylua,
    --
    --   -- You can also define your own configuration
    --   function()
    --     -- Full specification of configurations is down below and in Vim help
    --     -- files
    --     return {
    --       exe = "stylua",
    --       args = {
    --         "--search-parent-directories",
    --         "--stdin-filepath",
    --         util.escape_path(util.get_current_buffer_file_path()),
    --         "--",
    --         "-",
    --       },
    --       stdin = true,
    --     }
    --   end
    -- },

    -- Use the special "*" filetype for defining formatter configurations on
    -- any filetype
    ["*"] = {
      -- "formatter.filetypes.any" defines default configurations for any
      -- filetype
      require("formatter.filetypes.any").remove_trailing_whitespace
    }
  }
}

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd
augroup("__formatter__", { clear = true })
autocmd("BufWritePost", {
	group = "__formatter__",
	command = ":FormatWrite",
})
