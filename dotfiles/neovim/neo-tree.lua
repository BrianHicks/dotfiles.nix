-- remove deprecated commands from neo-tree 1.x
vim.cmd [[let g:neo_tree_remove_legacy_commands = 1]]

require("neo-tree").setup({
  open_files_do_not_replace_types = {"terminal", "trouble", "qf", "fugitive"},
  filesystem = {
    follow_current_file = true,
    group_empty_dirs = true,
  },
  buffers = {
    follow_current_file = true,
    group_empty_dirs = true,
  },
})

vim.keymap.set('n', '-', '<cmd>Neotree reveal=true position=left<CR>')
