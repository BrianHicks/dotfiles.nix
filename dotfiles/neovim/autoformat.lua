local util = require "formatter.util"

require("formatter").setup {
  logging = true,
  log_level = vim.log.levels.WARN,
  filetype = {
    -- https://github.com/mhartington/formatter.nvim/tree/master/lua/formatter/filetypes
    elm = {
      function()
	return {
	  exe = "elm-format",
	  args = { "--stdin" },
	  stdin = true,
	}
      end,
    },
    javascript = { require("formatter.filetypes.javascript").prettier },
    rust = { require("formatter.filetypes.rust").rustfmt },
    typescript = { require("formatter.filetypes.javascript").prettier },

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
