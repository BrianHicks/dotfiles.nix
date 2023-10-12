local actions = require("telescope.actions")
local trouble = require("trouble.providers.telescope")

local telescope = require("telescope")

telescope.setup {
  defaults = {
    mappings = {
      i = { ["<c-t>"] = trouble.open_with_trouble },
      n = { ["<c-t>"] = trouble.open_with_trouble },
    },
  },
  extensions = {
    ["ui-select"] = {
      require("telescope.themes").get_dropdown {},
    },
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
  },
}

telescope.load_extension("ui-select")
telescope.load_extension("fzf")

vim.keymap.set('n', '<leader>f', '<cmd>Telescope find_files hidden=true<CR>', { desc = 'find files' })
vim.keymap.set('n', '<leader>b', '<cmd>Telescope buffers<CR>', { desc = '[F]ind [B]uffers' })
vim.keymap.set('n', "<leader>'", '<cmd>Telescope resume<CR>', { desc = 'Resume last Telescope operation' })
vim.keymap.set('n', '<leader>l', '<cmd>Telescope current_buffer_fuzzy_find<CR>', { desc = '[F]ind by searching' })
vim.keymap.set('n', '<leader>/', '<cmd>Telescope live_grep<CR>', { desc = 'Live grep in project' })
vim.keymap.set('n', '<leader>?', '<cmd>Telescope grep_string<CR>', { desc = 'Find visual selection or highlight in project' })
vim.keymap.set('n', '<leader>`', '<cmd>Telescope builtin<CR>', { desc = '[F]ind [a]nything telescope can' })
