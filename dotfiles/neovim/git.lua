vim.keymap.set('n', '<leader>gs', '<cmd>Git<CR>', { desc = '[G]it overview' })
vim.keymap.set('n', '<leader>gb', '<cmd>Telescope git_branches<CR>', { desc = 'View [G]it [B]ranches' })
vim.keymap.set('n', '<leader>gC', '<cmd>Git commit -v<CR>', { desc = '[G]it [c]ommit' })
vim.keymap.set('n', '<leader>gc', ':Git commit -m ""<Left>', { desc = '[G]it [C]ommit inline' })
vim.keymap.set('n', '<leader>gr', '<cmd>Telescope git_bcommits<CR>', { desc = '[G]it [R]evert to commit' })
vim.keymap.set('n', '<leader>gm', '<cmd>Telescope git_status<CR>', { desc = '[G]it [M]odified files' })

-- vim-gitgutter
vim.g.gitgutter_map_keys = 0
vim.g.gitgutter_highlight_lines = 1
vim.keymap.set('n', '{', '<plug>(GitGutterPrevHunk)')
vim.keymap.set('n', '}', '<plug>(GitGutterNextHunk)')
vim.keymap.set('n', '<leader>gw', '<plug>(GitGutterStageHunk)', { desc = 'Stage hunk' })
vim.keymap.set('n', '<leader>gq', '<cmd>GitGutterQuickFix<CR>', { desc = 'Load hunks in quickfix' })
vim.keymap.set('n', '<leader>gX', '<plug>(GitGutterUndoHunk)', { desc = 'Undo (delete) hunk' })
vim.keymap.set('n', '<leader>gh', '<plug>(GitGutterPreviewHunk)', { desc = 'Preview hunk' })
