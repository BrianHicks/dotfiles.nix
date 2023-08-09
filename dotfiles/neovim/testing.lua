vim.g["test#strategy"] = "dispatch"

vim.keymap.set('n', '<leader>tf', '<cmd>TestNearest<CR>', { desc = 'test nearest' })
vim.keymap.set('n', '<leader>tt', '<cmd>TestFile<CR>', { desc = 'test file' })
vim.keymap.set('n', '<leader>ts', '<cmd>TestSuite<CR>', { desc = 'test suite' })
vim.keymap.set('n', '<leader>tl', '<cmd>TestLast<CR>', { desc = 'test last' })
vim.keymap.set('n', '<leader>to', '<cmd>TestVisit<CR>', { desc = 'test visit' })
