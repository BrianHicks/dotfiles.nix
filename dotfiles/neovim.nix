{ pkgs, ... }:

let
  plugins = pkgs.callPackage ./neovim/plugins.nix { };

  similar-sort = pkgs.callPackage ../pkgs/similar-sort { };

  nixfmt =
    import (fetchTarball "https://github.com/serokell/nixfmt/archive/e4f31f45799554ff378370256a24f606a3025b0a.tar.gz")
    { };
in {
  programs.neovim = {
    enable = true;

    # aliases
    viAlias = true;
    vimAlias = true;

    # runtimes
    withNodeJs = true;

    configure = {
      customRC = ''
        inoremap fd <ESC>
        nnoremap : ;
        nnoremap ; :

        let mapleader = " "
        let localleader = ","

        set clipboard=unnamed

        au FocusGained,BufEnter * :checktime

        set cursorline

        set inccommand=split

        "" MOUSE SUPPORT
        set mouse=a

        "" SPLITS
        nnoremap <c-j> <c-w><c-j>
        nnoremap <c-k> <c-w><c-k>
        nnoremap <c-l> <c-w><c-l>
        nnoremap <c-h> <c-w><c-h>

        set splitbelow
        set splitright

        " show stuff in the gutter
        set signcolumn=yes
        set number

        "" COLORS
        " note: possibly need instructions at https://github.com/rakr/vim-one if I ever add tmux stuff
        set termguicolors
        set background=dark
        let g:one_allow_italics = 1
        colorscheme one

        "" DELIMITERS
        let delimitMate_expand_space=1
        let delimitMate_expand_cr=1
        let delimitMate_nesting_quotes = ['"', '`']
        let backspace=2

        "" FIXING LITTLE ANNOYING THINGS
        " automatically change working directory when editing a file, so :edit
        " and friends work relatively. Causes problems with :make, which I will
        " fix later.
        " TODO: fix this to always work
        nnoremap <leader>cd :lcd %:p:h<CR>

        " automatically make missing directories on write
        augroup Mkdir
          autocmd!
          autocmd BufWritePre *
            \ if !isdirectory(expand("<afile>:p:h")) |
                \ call mkdir(expand("<afile>:p:h"), "p") |
            \ endif
        augroup END

        "" FZF
        " don't show fzf statusline
        autocmd! FileType fzf
        autocmd  FileType fzf set laststatus=0 noshowmode noruler
          \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

        "" STATUSLINE
        let g:lightline = {
          \ 'colorscheme': 'one'
          \ }

        let g:lightline.active = {
          \ 'left': [ [ 'mode', 'paste' ],
          \           [ 'readonly', 'filename', 'modified' ],
          \           [ 'gitbranch' ] ],
          \ 'right': [ [ 'lineinfo' ],
          \            [ 'percent' ],
          \            [ 'filetype' ] ]
          \ }

        let g:lightline.component_function = {
          \ 'gitbranch': 'fugitive#head',
          \ }

        set noshowmode

        "" GIT
        let g:gitgutter_git_executable = "${pkgs.git}/bin/git"
        let g:gitgutter_map_keys = 0
        set updatetime=100

        nmap gc :Gcommit<CR>

        nnoremap <leader>gb :Gblame<CR>
        nnoremap <leader>go :Gbrowse<CR>
        nnoremap <leader>gs :Gstatus<CR>
        nnoremap <leader>gc :Commits<CR>
        nnoremap <leader>gC :BCommmits<CR>
        nnoremap <leader>gW :Gwrite<CR>

        nnoremap <leader>gr :Gmove
        nnoremap <leader>gR :Gremove<CR>

        nmap <leader>gw <Plug>GitGutterStageHunk
        nmap <leader>gX <Plug>GitGutterUndoHunk
        nmap <leader>gp <Plug>GitGutterPreviewHunk
        nnoremap <leader>gf :GitGutterFold<CR>

        omap ic <Plug>GitGutterTextObjectInnerPending
        omap ac <Plug>GitGutterTextObjectOuterPending
        xmap ic <Plug>GitGutterTextObjectInnerVisual
        xmap ac <Plug>GitGutterTextObjectOuterVisual

        nmap ]c <Plug>GitGutterNextHunk
        nmap [c <Plug>GitGutterPrevHunk

        "" MARKDOWN
        " vim-markdown collapses and expands in a way that I don't like by default.
        let g:vim_markdown_folding_disabled = 1

        "" FORMATTING
        augroup fmt
          autocmd!
          " https://github.com/sbdchd/neoformat/issues/134
          au BufWritePre * try | undojoin | Neoformat | catch /^Vim\%((\a\+)\)\=:E790/ | finally | silent Neoformat | endtry
        augroup END

        let g:neoformat_nix_nixfmt = {
          \ 'exe': '${nixfmt}/bin/nixfmt',
          \ 'args': ['--width', '120'],
          \ 'stdin': 1,
          \ }

        let g:neoformat_enabled_nix = ['nixfmt']

        let g:neoformat_basic_format_trim = 1

        " https://github.com/sbdchd/neoformat/issues/251
        let g:neoformat_enabled_ruby = ['rufo', 'rubybeautify']

        "" DOCUMENTATION
        let g:doge_enable_mappings = 0
        nnoremap <silent> <leader>ed :DogeGenerate<CR>

        "" DISPATCH
        let g:dispatch_no_maps = 1

        nnoremap <silent> <leader>cc :Make<CR>
        nnoremap <silent> <leader>cC :Make!<CR>
        nnoremap          <leader>c<Space> :Make<Space>

        nnoremap <silent> <leader>co :Copen<CR>
        nnoremap <silent> <leader>cO :Copen!<CR>

        nnoremap <silent> <leader>cd :Dispatch<CR>
        nnoremap <silent> <leader>cD :Dispatch!<CR>
        nnoremap          <leader>cf :FocusDispatch<Space>
        nnoremap          <leader>c? :FocusDispatch<CR>

        nnoremap <leader>pp :Start<Space>
        nnoremap <leader>pP :Spawn<Space>

        "" YOINK
        nmap <c-n> <plug>(YoinkPostPasteSwapBack)
        nmap <c-p> <plug>(YoinkPostPasteSwapForward)

        nmap p <plug>(YoinkPaste_p)
        nmap P <plug>(YoinkPaste_P)

        "" TESTING
        let test#strategy = "dispatch"

        nnoremap <silent> <leader>tn :TestNearest<CR>
        nnoremap <silent> <leader>tf :TestFile<CR>
        nnoremap <silent> <leader>ta :TestSuite<CR>
        nnoremap <silent> <leader>tt :TestLast<CR>
        nnoremap <silent> gt :TestVisit<CR>

        "" LINTING
        let g:ale_cursor_detail = 1
        let g:ale_disable_lsp = 1
        let g:ale_echo_cursor = 0

        "" KEYBINDINGS
        " Where possible, keybindings act like normal vim bindings. Leader
        " keybindings are organized by action, then object (except for git,
        " which owns `g`)

        " bindings for fuzzy-finding
        nnoremap <silent> <C-t> :call fzf#run(fzf#wrap({"source": "git ls-files \| ${similar-sort}/bin/similar-sort " . @%,
                                                      \ "sink": "edit",
                                                      \ "options": "--tiebreak index"
                                                      \ }))<CR>
        nnoremap <silent> <S-t> :call fzf#run(fzf#wrap({"source": "git ls-files \| ${similar-sort}/bin/similar-sort " . @%,
                                                      \ "sink": "vsplit",
                                                      \ "options": "--tiebreak index"
                                                      \ }))<CR>
        nnoremap <silent> <A-t> :call fzf#run(fzf#wrap({"source": "git ls-files \| ${similar-sort}/bin/similar-sort " . @%,
                                                      \ "sink": "split",
                                                      \ "options": "--tiebreak index"
                                                      \ }))<CR>

        nnoremap <leader>ff :Files<CR>
        nnoremap <leader>fb :Buffers<CR>
        nnoremap <leader>ft :Ag<CR>
        nnoremap <leader>fl :Lines<CR>
        nnoremap <leader>fh :History<CR>
        nnoremap <leader>fH :Helptags<CR>

        " EDITING
        nnoremap <leader>eD :Delete<CR>
        nnoremap <leader>er :Rename
        nnoremap <leader>ec :Chmod
        nnoremap <leader>em :Mkdir
      '';

      packages.myVimPackage.start = plugins.all ++ [ pkgs.fzf ];
    };
  };
}
