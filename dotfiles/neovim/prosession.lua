vim.g.prosession_on_startup = 1
vim.g.prosession_per_branch = 1

require('telescope').load_extension('prosession')
vim.keymap.set('n', '<leader>q', '<cmd>Telescope prosession<CR>', { desc = 'Load sessions' })
