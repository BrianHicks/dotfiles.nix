{ pkgs, ... }:

let
  plugins = pkgs.callPackage ./neovim/plugins.nix { };

  brian-nur = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "BrianHicks";
    repo = "nur-packages";
    rev = "ff14246eb1403a0113d8819bcfbefa801decff3a";
    sha256 = "0gcrkpwqfarxycyignxj1x1sg7gi3swwnbwj92nwlzp010j8alcv";
  }) { };

  similar-sort = pkgs.callPackage ../pkgs/similar-sort { };
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
        set noshowmode

        "" LANGUAGE SERVER

        let g:LanguageClient_serverCommands = {
          \   'elm': ['${brian-nur.elm-language-server}/bin/elm-language-server', '--stdio'],
          \ }

        let g:LanguageClient_rootMarkers = {
          \   'elm': ['elm.json'],
          \ }

        let g:LanguageClient_changeThrottle = 0.25

        " this is the default, but do we want it? Time will tell.
        " let g:LanguageClient_selectionUI = "fzf"

        " augroup Autoformatter
        "   autocmd!
        "   " Setup formatexpr specified filetype(s).
        "   autocmd FileType elm setl formatexpr=CocAction('formatSelected')
        "   " Update signature help on jump placeholder
        "   autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
        " augroup end

        "" KEYBINDINGS
        " Where possible, keybindings act like normal vim bindings.
        " Leader keybindings are organized by object, then action. 

        " Use `[c` and `]c` to navigate diagnostics

        " bindings for fuzzy-finding
        nnoremap <silent> <leader>ff :call fzf#run(fzf#wrap({"source": "git ls-files \| ${similar-sort}/bin/similar-sort " . @%,
                                                           \ "sink": "edit",
                                                           \ "options": "--tiebreak index"
                                                           \ }))<CR>

        nnoremap <leader>fF :Files<CR>
        nnoremap <leader>bb :Buffers<CR>
        nnoremap <leader>fs :Ag<CR>
        nnoremap <leader>fl :Lines<CR>
        nnoremap <leader>fh :History<CR>
        nnoremap <leader>fc :Commits<CR>
        nnoremap <leader>fC :BCommmits<CR>
        nnoremap <leader>fH :Helptags<CR>

        " git
        nmap gc :Gcommit<CR>

        nnoremap <leader>gb :Gblame<CR>
        nnoremap <leader>go :Gbrowse<CR>
        nnoremap <leader>gs :Gstatus<CR>

        nnoremap <leader>gr :Gmove
        nnoremap <leader>gR :Gremove<CR>
        nnoremap <leader>gW :Gwrite<CR>

        nnoremap <leader>gl :Glog<CR>

        " editing
        nnoremap <leader>eD :Delete<CR>
        nnoremap <leader>er :Rename
        nnoremap <leader>ec :Chmod
        nnoremap <leader>em :Mkdir
      '';

      packages.myVimPackage.start = plugins.all ++ [ pkgs.fzf ];
    };
  };
}
