-- Leaders need to be mapped before requiring anything so they will be
-- correctly bound everywhere.
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- misc options
vim.wo.number = true
vim.o.mouse = 'a'
vim.o.breakindent = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.wo.signcolumn = 'yes'
vim.o.termguicolors = true

-- swap ; and : to save a keystroke and reduce wear on the pinky finger
vim.keymap.set('n', ';', ':')
vim.keymap.set('n', ':', ';')

-- deal with word wrap nicely
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

require("better_escape").setup {
  mapping = {"fd"},
  -- TODO: clear_empty_lines?
}

-- make an easier way to exit insert and write
vim.keymap.set('i', '<C-e>', '<esc>:w<cr>') -- TODO: do :wq in a fugitive commit buffer

-- which-key
vim.o.timeout = true
vim.o.timeoutlen = 300
require("which-key").setup()

-- comments
require('Comment').setup()

-- theming
vim.cmd('colorscheme nightfox')

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

-- surround
require('nvim-surround').setup()

-- tree-sitter
require('nvim-treesitter.configs').setup {
  -- grammars are taken care of by Nix
  highlight = { enable = true },
  incremental_selection = { enable = true },
  textobjects = { enable = true },

  -- the Markdown parser currently has some issue in my main work repo that
  -- makes neovim quit (segfault?) when it tries to load the README.
  disable = { "markdown" },
}

-- LSP
-- NOTE: language servers are set up in default.nix!

vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<leader>d', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    local nmap = function(keys, func, desc)
      if desc then
        desc = 'LSP: ' .. desc
      end

      vim.keymap.set('n', keys, func, { buffer = ev.buf, desc = desc })
    end

    -- Buffer-local mappings
    nmap('<leader>r', vim.lsp.buf.rename, '[R]ename')
    nmap('<leader>a', vim.lsp.buf.code_action, 'Code [a]ction')

    nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
    nmap('gr', '<cmd>Telescope lsp_references<CR>', '[G]oto [R]eferences')
    nmap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
    nmap('gD', vim.lsp.buf.type_definition, '[G]oto type [D]efinition')
    nmap('<leader>s', '<cmd>Telescope lsp_document_symbols<CR>', 'Document [S]ymbols')
    nmap('<leader>S', '<cmd>Telescope lsp_dynamic_workspace_symbols<CR>', 'Workspace [S]ymbols')
    nmap('K', vim.lsp.buf.hover, 'Hover documentation')

    vim.api.nvim_buf_create_user_command(ev.buf, 'Format', function(_)
      vim.lsp.buf.format()
    end, { desc = 'Format current buffer with LSP' })
 end,

 require("fidget").setup({})
})

-- completion
local cmp = require('cmp')
local luasnip = require('luasnip')
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup({})

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete {},
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),
  sources = cmp.config.sources(
    {
      { name = 'nvim_lsp' },
      { name = 'luasnip' },
    },
    {
      { name = 'buffer' },
    }
  ),
})

-- get better completions in 
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources(
    { { name = 'git' } },
    { { name = 'buffer' } }
  )
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = { { name = 'buffer' } },
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources(
    { { name = 'path' } },
    { { name = 'cmdline' } }
  )
})

-- status line
require('lualine').setup()

-- highlight on yank
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})
