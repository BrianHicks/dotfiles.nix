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
}

vim.keymap.set('n', '<leader>f', ':Telescope find_files<CR>', { desc = 'find files' })
vim.keymap.set('n', '<leader>b', ':Telescope buffers<CR>', { desc = '[F]ind [B]uffers' })
vim.keymap.set('n', "<leader>'", ':Telescope resume<CR>', { desc = 'Resume last Telescope operation' })
vim.keymap.set('n', '<leader>l', ':Telescope current_buffer_fuzzy_find<CR>', { desc = '[F]ind by searching' })
vim.keymap.set('n', '<leader>/', ':Telescope live_grep<CR>', { desc = '[F]ind in [P]roject' })
vim.keymap.set('n', '<leader>s', ':Telescope builtin<CR>', { desc = '[F]ind [a]nything telescope can' })
