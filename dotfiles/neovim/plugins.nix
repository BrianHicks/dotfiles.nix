{ pkgs ? import <nixpkgs> { }, ... }:

let sources = import ../../nix/sources.nix;
in rec {
  LnL7.vim-nix = pkgs.vimUtils.buildVimPlugin {
    name = "vim-nix";
    src = sources.vim-nix;
  };

  Raimondi.delimitMate = pkgs.vimUtils.buildVimPlugin {
    name = "delimitMate";
    src = sources.delimitMate;
    buildInputs = [ pkgs.zip pkgs.vim ];
  };

  Shougo.neco-syntax = pkgs.vimUtils.buildVimPlugin {
    name = "neco-syntax";
    src = sources.neco-syntax;
  };

  airblade.vim-gitgutter = pkgs.vimUtils.buildVimPlugin {
    name = "vim-gitgutter";
    src = sources.vim-gitgutter;
  };

  andys8.vim-elm-syntax = pkgs.vimUtils.buildVimPlugin {
    name = "vim-elm-syntax";
    src = sources.vim-elm-syntax;
  };

  dense-analysis.ale = pkgs.vimUtils.buildVimPlugin {
    name = "ale";
    src = sources.ale;
    patches = [
      (pkgs.fetchpatch {
        url = "https://patch-diff.githubusercontent.com/raw/dense-analysis/ale/pull/2750.patch";
        sha256 = "075zrk6njjya62kzkr24px9l02n77si50z2pnxnm03l2cmrm3ffw";
      })
    ];
  };

  farmergreg.vim-lastplace = pkgs.vimUtils.buildVimPlugin {
    name = "vim-lastplace";
    src = sources.vim-lastplace;
  };

  fgrsnau.ncm2-otherbuf = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-otherbuf";
    src = sources.ncm2-otherbuf;
  };

  glts.vim-magnum = pkgs.vimUtils.buildVimPlugin {
    name = "vim-magnum";
    src = sources.vim-magnum;
  };

  glts.vim-radical = pkgs.vimUtils.buildVimPlugin {
    name = "vim-radical";
    src = sources.vim-radical;
  };

  godlygeek.tabular = pkgs.vimUtils.buildVimPlugin {
    name = "tabular";
    src = sources.tabular;
  };

  itchyny."lightline.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "lightline.vim";
    src = sources."lightline.vim";
  };

  janko.vim-test = pkgs.vimUtils.buildVimPlugin {
    name = "vim-test";
    src = sources.vim-test;
  };

  junegunn."fzf.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "fzf.vim";
    src = sources."fzf.vim";
  };

  kassio.neoterm = pkgs.vimUtils.buildVimPlugin {
    name = "neoterm";
    src = sources.neoterm;
  };

  kkoomen.vim-doge = pkgs.vimUtils.buildVimPlugin {
    name = "vim-doge";
    src = sources.vim-doge;
  };

  maximbaz.lightline-ale = pkgs.vimUtils.buildVimPlugin {
    name = "lightline-ale";
    src = sources.lightline-ale;
  };

  mileszs."ack.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "ack.vim";
    src = sources."ack.vim";
  };

  mityu.vim-applescript = pkgs.vimUtils.buildVimPlugin {
    name = "vim-applescript";
    src = sources.vim-applescript;
  };

  ncm2.ncm2 = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2";
    src = sources.ncm2;
  };

  ncm2.ncm2-bufword = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-bufword";
    src = sources.ncm2-bufword;
  };

  ncm2.ncm2-path = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-path";
    src = sources.ncm2-path;
  };

  ncm2.ncm2-syntax = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-syntax";
    src = sources.ncm2-syntax;
  };

  plasticboy.vim-markdown = pkgs.vimUtils.buildVimPlugin {
    name = "vim-markdown";
    src = sources.vim-markdown;
  };

  rakr.vim-one = pkgs.vimUtils.buildVimPlugin {
    name = "vim-one";
    src = sources.vim-one;
  };

  roxma.nvim-yarp = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-yarp";
    src = sources.nvim-yarp;
  };

  sbdchd.neoformat = pkgs.vimUtils.buildVimPlugin {
    name = "neoformat";
    src = sources.neoformat;
  };

  stefandtw."quickfix-reflector.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "quickfix-reflector.vim";
    src = sources."quickfix-reflector.vim";
  };

  svermeulen.ncm2-yoink = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-yoink";
    src = sources.ncm2-yoink;
  };

  svermeulen.vim-subversive = pkgs.vimUtils.buildVimPlugin {
    name = "vim-subversive";
    src = sources.vim-subversive;
  };

  svermeulen.vim-yoink = pkgs.vimUtils.buildVimPlugin {
    name = "vim-yoink";
    src = sources.vim-yoink;
  };

  tommcdo.vim-exchange = pkgs.vimUtils.buildVimPlugin {
    name = "vim-exchange";
    src = sources.vim-exchange;
  };

  tpope.vim-abolish = pkgs.vimUtils.buildVimPlugin {
    name = "vim-abolish";
    src = sources.vim-abolish;
  };

  tpope.vim-bundler = pkgs.vimUtils.buildVimPlugin {
    name = "vim-bundler";
    src = sources.vim-bundler;
  };

  tpope.vim-commentary = pkgs.vimUtils.buildVimPlugin {
    name = "vim-commentary";
    src = sources.vim-commentary;
  };

  tpope.vim-dispatch = pkgs.vimUtils.buildVimPlugin {
    name = "vim-dispatch";
    src = sources.vim-dispatch;
  };

  tpope.vim-eunuch = pkgs.vimUtils.buildVimPlugin {
    name = "vim-eunuch";
    src = sources.vim-eunuch;
  };

  tpope.vim-fugitive = pkgs.vimUtils.buildVimPlugin {
    name = "vim-fugitive";
    src = sources.vim-fugitive;
  };

  tpope.vim-projectionist = pkgs.vimUtils.buildVimPlugin {
    name = "vim-projectionist";
    src = sources.vim-projectionist;
  };

  tpope.vim-rails = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rails";
    src = sources.vim-rails;
  };

  tpope.vim-rake = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rake";
    src = sources.vim-rake;
  };

  tpope.vim-repeat = pkgs.vimUtils.buildVimPlugin {
    name = "vim-repeat";
    src = sources.vim-repeat;
  };

  tpope.vim-rhubarb = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rhubarb";
    src = sources.vim-rhubarb;
  };

  tpope.vim-rsi = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rsi";
    src = sources.vim-rsi;
  };

  tpope.vim-sensible = pkgs.vimUtils.buildVimPlugin {
    name = "vim-sensible";
    src = sources.vim-sensible;
  };

  tpope.vim-sleuth = pkgs.vimUtils.buildVimPlugin {
    name = "vim-sleuth";
    src = sources.vim-sleuth;
  };

  tpope.vim-surround = pkgs.vimUtils.buildVimPlugin {
    name = "vim-surround";
    src = sources.vim-surround;
  };

  tpope.vim-unimpaired = pkgs.vimUtils.buildVimPlugin {
    name = "vim-unimpaired";
    src = sources.vim-unimpaired;
  };

  tpope.vim-vinegar = pkgs.vimUtils.buildVimPlugin {
    name = "vim-vinegar";
    src = sources.vim-vinegar;
  };

  vim-ruby.vim-ruby = pkgs.vimUtils.buildVimPlugin {
    name = "vim-ruby";
    src = sources.vim-ruby;
  };

  wellle."targets.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "targets.vim";
    src = sources."targets.vim";
  };

  wellle."visual-split.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "visual-split.vim";
    src = sources."visual-split.vim";
  };

  all = [
    LnL7.vim-nix
    Raimondi.delimitMate
    Shougo.neco-syntax
    airblade.vim-gitgutter
    andys8.vim-elm-syntax
    dense-analysis.ale
    farmergreg.vim-lastplace
    fgrsnau.ncm2-otherbuf
    glts.vim-magnum
    glts.vim-radical
    godlygeek.tabular
    itchyny."lightline.vim"
    janko.vim-test
    junegunn."fzf.vim"
    kassio.neoterm
    kkoomen.vim-doge
    maximbaz.lightline-ale
    mileszs."ack.vim"
    mityu.vim-applescript
    ncm2.ncm2
    ncm2.ncm2-bufword
    ncm2.ncm2-path
    ncm2.ncm2-syntax
    plasticboy.vim-markdown
    rakr.vim-one
    roxma.nvim-yarp
    sbdchd.neoformat
    stefandtw."quickfix-reflector.vim"
    svermeulen.ncm2-yoink
    svermeulen.vim-subversive
    svermeulen.vim-yoink
    tommcdo.vim-exchange
    tpope.vim-abolish
    tpope.vim-bundler
    tpope.vim-commentary
    tpope.vim-dispatch
    tpope.vim-eunuch
    tpope.vim-fugitive
    tpope.vim-projectionist
    tpope.vim-rails
    tpope.vim-rake
    tpope.vim-repeat
    tpope.vim-rhubarb
    tpope.vim-rsi
    tpope.vim-sensible
    tpope.vim-sleuth
    tpope.vim-surround
    tpope.vim-unimpaired
    tpope.vim-vinegar
    vim-ruby.vim-ruby
    wellle."targets.vim"
    wellle."visual-split.vim"
  ];
}
