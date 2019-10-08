{ pkgs, lib, ... }:

let
  similar-sort = pkgs.callPackage ../pkgs/similar-sort { };

  sources = import ../nix/sources.nix;

  nixfmt = import sources.nixfmt { };

  brianhicks-nur = import sources.brianhicks-nur { };

  vimSources = lib.filterAttrs (_: source: lib.hasAttrByPath [ "vim" ] source) sources;

  unpatched = lib.mapAttrs (name: source:
    pkgs.vimUtils.buildVimPlugin {
      name = name;
      src = source;
    }) vimSources;

  plugins = unpatched // {
    ale = unpatched.ale.overrideAttrs (attrs: {
      patches = [
        (pkgs.fetchpatch {
          url = "https://patch-diff.githubusercontent.com/raw/dense-analysis/ale/pull/2750.patch";
          sha256 = "075zrk6njjya62kzkr24px9l02n77si50z2pnxnm03l2cmrm3ffw";
        })
      ];
    });

    delimitMate = unpatched.delimitMate.overrideAttrs (attrs: { buildInputs = [ pkgs.zip pkgs.vim ]; });

    "deoplete.nvim" = unpatched."deoplete.nvim".overrideAttrs (attrs: {
      # deoplete has a Makefile but it looks like it's only for test stuff, so
      # we can just ignore it. The plugin should be usable as-checked-in.
      buildPhase = "true";
    });
  };
in {
  programs.neovim = {
    enable = true;

    # aliases
    viAlias = true;
    vimAlias = true;

    # runtimes
    withNodeJs = true;

    plugins = (lib.mapAttrsToList (_: plugin: plugin) plugins) ++ [ pkgs.fzf ];

    extraConfig = ''
      inoremap fd <ESC>
      nnoremap : ;
      nnoremap ; :

      let mapleader = " "
      let localleader = ","

      au FocusGained,BufEnter * :checktime

      let showbreak= '↪ '
      set clipboard=unnamed
      set cursorline
      set hidden
      set ignorecase
      set inccommand=split
      set smartcase
      set undodir=~/.config/vim/undo
      set undofile

      "" quickfix window
      autocmd FileType qf setlocal nonumber
      autocmd FileType qf wincmd L | vertical resize 80
      let g:dispatch_quickfix_height = 9999

      nnoremap <Leader>qq :cwindow<CR>
      nnoremap <Leader>qo :copen<CR>

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
      function! s:base16_customize() abort
        " For palenight, darken status line, numbers, line column. Too bright for me by default.
        call Base16hi("SignColumn", g:base16_gui03, g:base16_gui02, g:base16_cterm03, g:base16_cterm02, "", "")
        call Base16hi("LineNr", g:base16_gui03, g:base16_gui02, g:base16_cterm03, g:base16_cterm02, "", "")
        call Base16hi("CursorLine", "", g:base16_gui02, "", g:base16_cterm02, "", "")
        call Base16hi("CursorLineNr",  g:base16_gui04, g:base16_gui02, g:base16_cterm04, g:base16_cterm02, "", "")

        " Brighten base text by default--it's a little light.
        call Base16hi("Normal", "CBD4E6", g:base16_gui00, g:base16_cterm06, g:base16_cterm00, "", "")
        call Base16hi("Comment", g:base16_gui05, "", g:base16_gui05, "", "", "")
        call Base16hi("Visual", "", "4C5167", "", g:base16_cterm02, "", "")

        " customize Elm highlighting
        "
        "   import Foo.Bar.Baz as Baz
        "   ****** ^^^^^^^.&&& ** &&&
        "
        "   type alias Quux =
        "   !!!! @@@@@ &&&& #
        "       { blah : Int
        "       @ %%%% # &&&
        "       }
        "       @
        "
        " ! Typedef
        " # Operator
        " % elmTypeLevelTypedef
        " & Identifier
        " * Include
        " . Normal
        " @ Delimiter
        " ^ Type
        "
        call Base16hi("Delimiter", g:base16_gui06, "", g:base16_cterm06, "", "", "")
        call Base16hi("Identifier", g:base16_gui0C, "", g:base16_cterm0C, "", "", "")
        call Base16hi("Type", g:base16_gui0C, "", g:base16_cterm0C, "", "", "")
      endfunction

      augroup on_change_colorschema
        autocmd!
        autocmd ColorScheme * call s:base16_customize()
      augroup END

      set termguicolors
      set background=dark
      colorscheme base16-material-palenight

      function! SynGroup()
        let l:s = synID(line('.'), col('.'), 1)
        echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
      endfun

      command SynGroup :call SynGroup()

      "" DELIMITERS
      let delimitMate_expand_space=1
      let delimitMate_expand_cr=1
      let delimitMate_nesting_quotes = ['"', '`']
      let backspace=2

      "" FIXING LITTLE ANNOYING THINGS
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
        \ 'colorscheme': 'base16_material_palenight'
        \ }

      let g:lightline.active = {
        \ 'left': [ [ 'mode', 'paste' ],
        \           [ 'readonly', 'filename', 'modified' ],
        \           [ 'gitbranch' ] ],
        \ 'right': [ [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ],
        \            [ 'lineinfo' ],
        \            [ 'percent' ],
        \            [ 'filetype' ] ]
        \ }

      let g:lightline.component_function = {
        \ 'gitbranch': 'fugitive#head',
        \ }
        
      let g:lightline.component_expand = {
        \ 'linter_checking': 'lightline#ale#checking',
        \ 'linter_warnings': 'lightline#ale#warnings',
        \ 'linter_errors': 'lightline#ale#errors',
        \ 'linter_ok': 'lightline#ale#ok',
        \ }

      let g:lightline.component_type = {
        \ 'linter_checking': 'left',
        \ 'linter_warnings': 'warning',
        \ 'linter_errors': 'error',
        \ 'linter_ok': 'left',
        \ }

      set noshowmode

      "" GIT
      let g:gitgutter_git_executable = "${pkgs.git}/bin/git"
      let g:gitgutter_map_keys = 0
      set updatetime=100

      nnoremap <leader>gb :Gblame<CR>
      nnoremap <leader>go :Gbrowse<CR>
      nnoremap <leader>gs :Gstatus<CR>
      nnoremap <leader>gc :Gcommit --verbose<CR>
      nnoremap <leader>gW :Gwrite<CR>

      nnoremap <leader>gr :Gmove
      nnoremap <leader>gR :Gremove<CR>

      nmap <leader>gw <Plug>(GitGutterStageHunk)
      nmap <leader>gX <Plug>(GitGutterUndoHunk)
      nmap <leader>gp <Plug>(GitGutterPreviewHunk)
      nnoremap <leader>gf :GitGutterFold<CR>

      omap ic <Plug>(GitGutterTextObjectInnerPending)
      omap ac <Plug>(GitGutterTextObjectOuterPending)
      xmap ic <Plug>(GitGutterTextObjectInnerVisual)
      xmap ac <Plug>(GitGutterTextObjectOuterVisual)

      nmap ]c <Plug>(GitGutterNextHunk)
      nmap [c <Plug>(GitGutterPrevHunk)

      "" MARKDOWN
      " vim-markdown collapses and expands in a way that I don't like by default.
      let g:vim_markdown_folding_disabled = 1

      "" FORMATTING
      augroup fmt
        autocmd!
        " maybe needs https://github.com/sbdchd/neoformat/issues/134 again in the future. We'll see.
        autocmd BufWritePre *.elm undojoin | Neoformat
        autocmd BufWritePre *.hs  undojoin | Neoformat
        autocmd BufWritePre *.js  undojoin | Neoformat
        autocmd BufWritePre *.nix undojoin | Neoformat
      augroup END

      nnoremap <leader>ef :Neoformat<CR>

      let g:neoformat_nix_nixfmt = {
        \ 'exe': '${nixfmt}/bin/nixfmt',
        \ 'args': ['--width', '120'],
        \ 'stdin': 1,
        \ }

      let g:neoformat_enabled_nix = ['nixfmt']

      let g:neoformat_basic_format_trim = 1

      " the formatters do not respect the indent levels in some of the files
      " I work with regularly.
      let g:neoformat_enabled_json = []

      " https://github.com/sbdchd/neoformat/issues/251
      let g:neoformat_enabled_ruby = ['rufo', 'rubybeautify']

      " I want to use prettier before anything else for JS code
      let g:neoformat_enabled_javascript = [ 'prettier', 'prettier-eslint', 'js-beautify', 'prettydiff', 'clang-format', 'esformatter', 'standard' ]

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

      let g:test#ruby#rspec#options = '--format documentation --profile 30'

      "" TERMINAL
      let g:neoterm_default_mod = "vsplit"
      let g:neoterm_autoinsert = 1

      nnoremap <silent> <leader>' :<c-u>exec v:count.'Topen'<CR>
      tnoremap fd <c-\><c-n>
      tnoremap <ESC> <c-\><c-n>

      tmap <c-j> <c-\><c-n><c-w><c-j>
      tmap <c-k> <c-\><c-n><c-w><c-k>
      tmap <c-l> <c-\><c-n><c-w><c-l>
      tmap <c-h> <c-\><c-n><c-w><c-h>

      "" LINTING / LANGUAGE SERVER
      let g:ale_cursor_detail = 0
      let g:ale_echo_cursor = 1

      nnoremap <silent> [e :ALEPreviousWrap<CR>
      nnoremap <silent> [E :ALEFirst<CR>
      nnoremap <silent> ]e :ALENextWrap<CR>
      nnoremap <silent> ]E :ALELast<CR>
      nnoremap <silent> ge :ALEDetail<CR>
      nnoremap <silent> <leader>eF :ALEFix<CR>

      let g:ale_fixers = {
        \ 'ruby': [ 'rubocop' ]
        \ }

      let g:ale_linters = {
        \ 'elm': [ 'elm_ls' ]
        \ }

      " ALE in Elm
      let g:ale_disable_lsp = 0
      let g:ale_elm_ls_use_global = 1
      let g:ale_elm_ls_executable = "${brianhicks-nur.elm-language-server}/bin/elm-language-server"
      let g:ale_elm_ls_elm_analyse_trigger = 'never'

      autocmd FileType elm nmap gd <Plug>(ale_go_to_definition)
      autocmd FileType elm nmap gr <Plug>(ale_find_references)
      autocmd FileType elm nmap K <Plug>(ale_hover)

      "" REPLACEMENT
      nmap s <plug>(SubversiveSubvertRange)
      xmap s <plug>(SubversiveSubvertRange)
      nmap <leader>ss <plug>(SubversiveSubvertWordRange)

      "" FINDING STUFF

      let g:ackprg = '${pkgs.ag}/bin/ag --vimgrep'

      " bindings for fuzzy-finding
      nnoremap <silent> <C-t> :call fzf#run(fzf#wrap({"source": "git ls-files --others --cached --exclude-standard \| ${similar-sort}/bin/similar-sort " . @%,
                                                    \ "sink": "edit",
                                                    \ "options": "--tiebreak index"
                                                    \ }))<CR>
      nnoremap <silent> <S-t> :call fzf#run(fzf#wrap({"source": "git ls-files --others --cached --exclude-standard \| ${similar-sort}/bin/similar-sort " . @%,
                                                    \ "sink": "vsplit",
                                                    \ "options": "--tiebreak index"
                                                    \ }))<CR>
      nnoremap <silent> <A-t> :call fzf#run(fzf#wrap({"source": "git ls-files --others --cached --exclude-standard \| ${similar-sort}/bin/similar-sort " . @%,
                                                    \ "sink": "split",
                                                    \ "options": "--tiebreak index"
                                                    \ }))<CR>

      nnoremap <leader>fb :Buffers<CR>
      nnoremap <leader>ff :Files<CR>
      nnoremap <leader>fH :Helptags<CR>
      nnoremap <leader>fh :History<CR>
      nnoremap <leader>fj :BLines<CR>
      nnoremap <leader>fl :Lines<CR>
      nnoremap <leader>ft :Ack<Space>

      nmap <leader><leader> <plug>(fzf-maps-n)
      xmap <leader><leader> <plug>(fzf-maps-x)
      omap <leader><leader> <plug>(fzf-maps-o)

      "" EDITING
      nnoremap <leader>eD :Delete<CR>
      nnoremap <leader>er :Rename
      nnoremap <leader>ec :Chmod
      nnoremap <leader>em :Mkdir

      "" TEXT OBJECTS
      onoremap ib :exec "normal! ggVG"<CR>
      onoremap iv :exec "normal! HVL"<CR>

      "" COMPLETION
      let g:deoplete#enable_at_startup = 1
      let g:deoplete#num_processes = 1
      let g:deoplete#max_list = 25

      " I don't like accepting completion suggestions with <CR>. I'd rather
      " use <Tab>.
      inoremap <expr> <CR> pumvisible() ? "\<C-e>\<CR>" : "\<CR>"
    '';
  };
}
