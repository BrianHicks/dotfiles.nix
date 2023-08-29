-- git
local gitsigns = require('gitsigns')
gitsigns.setup()
gitsigns.toggle_numhl()

vim.keymap.set('n', '<leader>gs', '<cmd>Git<CR>', { desc = '[G]it overview' })
vim.keymap.set('n', '<leader>gb', '<cmd>Telescope git_branches<CR>', { desc = 'View [G]it [B]ranches' })
vim.keymap.set('n', '<leader>gh', gitsigns.preview_hunk_inline, { desc = 'Preview [G]it [H]unk' })
vim.keymap.set('n', '<leader>gw', gitsigns.stage_hunk, { desc = '[G]it [S]tage hunk' })
vim.keymap.set('n', '<leader>gW', gitsigns.stage_buffer, { desc = '[G]it [S]tage buffer' })
vim.keymap.set('n', '<leader>gu', gitsigns.undo_stage_hunk, { desc = '[G]it [U]ndo stage hunk' })
vim.keymap.set('n', '<leader>gC', '<cmd>Git commit -v<CR>', { desc = '[G]it [c]ommit' })
vim.keymap.set('n', '<leader>gc', ':Git commit -m ""<Left>', { desc = '[G]it [C]ommit inline' })
vim.keymap.set('n', '<leader>gr', '<cmd>Telescope git_bcommits<CR>', { desc = '[G]it [R]evert to commit' })
vim.keymap.set('n', '<leader>gm', '<cmd>Telescope git_status<CR>', { desc = '[G]it [M]odified files' })

vim.keymap.set('n', '{', require('gitsigns').prev_hunk)
vim.keymap.set('n', '}', require('gitsigns').next_hunk)
