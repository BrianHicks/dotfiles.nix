{ pkgs, ... }:

rec {

  ElmCast.elm-vim = pkgs.vimUtils.buildVimPlugin {
    name = "elm-vim";
    src = pkgs.fetchFromGitHub {
      owner = "ElmCast";
      repo = "elm-vim";
      rev = "165107a9fd2b20c8f050fc4f977b4e41c790b1e7";
      sha256 = "0gf7b49by0ybx3ndz7sz5dwcfnps4sz6wsr02lyarj8f8116ysy5";
    };
  };

  LnL7.vim-nix = pkgs.vimUtils.buildVimPlugin {
    name = "vim-nix";
    src = pkgs.fetchFromGitHub {
      owner = "LnL7";
      repo = "vim-nix";
      rev = "a3eed01f4de995a51dfdd06287e44fcb231f6adf";
      sha256 = "0pwdfwws1dj3705m00ghw3dvym5zbm00bfsj023gmbp6vr8wn6yi";
    };
  };

  Raimondi.delimitMate = pkgs.vimUtils.buildVimPlugin {
    name = "delimitMate";
    src = pkgs.fetchFromGitHub {
      owner = "Raimondi";
      repo = "delimitMate";
      rev = "728b57a6564c1d2bdfb9b9e0f2f8c5ba3d7e0c5c";
      sha256 = "0fskm9gz81dk8arcidrm71mv72a7isng1clssqkqn5wnygbiimsn";
    };
    buildInputs = [ pkgs.zip pkgs.vim ];
  };

  farmergreg.vim-lastplace = pkgs.vimUtils.buildVimPlugin {
    name = "vim-lastplace";
    src = pkgs.fetchFromGitHub {
      owner = "farmergreg";
      repo = "vim-lastplace";
      rev = "fbb88789b531e1fc1abe25b2f44f4f4c8a73f14d";
      sha256 = "0661dnm0idaqy28pw03fznq5hpl2pbb4r0c1gvdmf59ywhsa2dav";
    };
  };

  itchyny."lightline.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "lightline.vim";
    src = pkgs.fetchFromGitHub {
      owner = "itchyny";
      repo = "lightline.vim";
      rev = "f5039419d87b76accee7000319b394ce25a0dbfb";
      sha256 = "0cfjw1jpddw92jz62ly8m6waxknj19cazff01x8drk1lr9xj6wdy";
    };
  };

  junegunn."fzf.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "fzf.vim";
    src = pkgs.fetchFromGitHub {
      owner = "junegunn";
      repo = "fzf.vim";
      rev = "359a80e3a34aacbd5257713b6a88aa085337166f";
      sha256 = "0a01g0gw59m57zizidvm9n89qh8lvj0aq4kyxy27i72ysclp3wsf";
    };
  };

  neoclide."coc.nvim" = pkgs.vimUtils.buildVimPlugin {
    name = "coc.nvim";
    src = pkgs.fetchFromGitHub {
      owner = "neoclide";
      repo = "coc.nvim";
      rev = "v0.0.73";
      sha256 = "1z7573rbh806nmkh75hr1kbhxr4jysv6k9x01fcyjfwricpa3cf7";
    };
  };

  rakr.vim-one = pkgs.vimUtils.buildVimPlugin {
    name = "vim-one";
    src = pkgs.fetchFromGitHub {
      owner = "rakr";
      repo = "vim-one";
      rev = "6695e135415c53a8fbe96672e382200aa1ffb4b4";
      sha256 = "0951r9kw23zlwd7fda6ib2d9k0akpfd2hvh82pasgw3ks9v1n2vf";
    };
  };

  tpope.vim-abolish = pkgs.vimUtils.buildVimPlugin {
    name = "vim-abolish";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-abolish";
      rev = "b95463a1cffd8fc9aff2a1ff0ae9327944948699";
      sha256 = "1cvhylz6hgvl63zhlrxqrjqqp07pm29i436xv33dzzhdp8dcj1mp";
    };
  };

  tpope.vim-bundler = pkgs.vimUtils.buildVimPlugin {
    name = "vim-bundler";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-bundler";
      rev = "c4553a999c77e48bf74f43ac3d3503456bbba5a5";
      sha256 = "1wmvxisi1afbqmd35yv8cg38p5sg1d1q2nd5g3f4h3ac58v84bl1";
    };
  };

  tpope.vim-endwise = pkgs.vimUtils.buildVimPlugin {
    name = "vim-endwise";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-endwise";
      rev = "f67d022169bd04d3c000f47b1c03bfcbc4209470";
      sha256 = "0lq2sphh2mfciva184b4b3if202hr4yls4d2gzbjx7ibch45zb9i";
    };
  };

  tpope.vim-fugitive = pkgs.vimUtils.buildVimPlugin {
    name = "vim-fugitive";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-fugitive";
      rev = "bc3d36e559e446d963e0b1dd832d3b39ea233241";
      sha256 = "1072ldcvp1kq5kckzxvr6g6qijf2a6ccrig1v63jydpdqmn8h6kq";
    };
  };

  tpope.vim-projectionist = pkgs.vimUtils.buildVimPlugin {
    name = "vim-projectionist";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-projectionist";
      rev = "94001f00825c36fab63b9f3ca47a204111e561a2";
      sha256 = "0pppaavma07c3lxqlbk8cghdsirncxng52mjmv5qk8yar8kxqvbr";
    };
  };

  tpope.vim-rails = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rails";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-rails";
      rev = "2b33a3df78f337ec44874fa9fb2862ab229614ce";
      sha256 = "06nhfbj410lbfn1ik3hdg0xd4533xn2d7ir5c4imra211svgyzwx";
    };
  };

  tpope.vim-rake = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rake";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-rake";
      rev = "673289cc8a620765afb51a884589cd5258b2e396";
      sha256 = "1mhsn33myajwwkfyf6kspy6gvsx1264f237mdvzprx3npazada0p";
    };
  };

  tpope.vim-repeat = pkgs.vimUtils.buildVimPlugin {
    name = "vim-repeat";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-repeat";
      rev = "ae361bea990e27d5beade3a8d9fa22e25cec3100";
      sha256 = "0myqas20r81gfvfrph4ww38db1nz32qlph7syr6ym8zl7c1fcjd4";
    };
  };

  tpope.vim-sensible = pkgs.vimUtils.buildVimPlugin {
    name = "vim-sensible";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-sensible";
      rev = "c176d137892f33945d3d4dd766fd21611e9b5ddf";
      sha256 = "11adqaccwph4z5a4kyycd1gbc1l9np4za0d4fbd3cnh1zqf2xzjz";
    };
  };

  tpope.vim-sleuth = pkgs.vimUtils.buildVimPlugin {
    name = "vim-sleuth";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-sleuth";
      rev = "7a104e34c10c6f3581c6e98da7834d765d0b067c";
      sha256 = "0i147vhrrkarir36ysyaic42d22hk38cnpaqzqck7b2zdwnqrvbv";
    };
  };

  tpope.vim-surround = pkgs.vimUtils.buildVimPlugin {
    name = "vim-surround";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-surround";
      rev = "fab8621670f71637e9960003af28365129b1dfd0";
      sha256 = "0lkc0isv1cqv34qfia9mjvnp1nzz0qqy3k47z8r3xzb7dxgymkw8";
    };
  };

  tpope.vim-unimpaired = pkgs.vimUtils.buildVimPlugin {
    name = "vim-unimpaired";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-unimpaired";
      rev = "ab7082c0e89df594a5ba111e18af17b3377d216d";
      sha256 = "1gvzjihkxnc84kd7sdh26kmm0rqi19xmwiisfqhf307yqyqa6lkj";
    };
  };

  tpope.vim-vinegar = pkgs.vimUtils.buildVimPlugin {
    name = "vim-vinegar";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-vinegar";
      rev = "09ac84c4d152a944caa341e913220087211c72ef";
      sha256 = "18ki85s1l4f0q40k26jvcdcbq6a73x870dnxkw20ji3pfwdaa5v3";
    };
  };

  vim-ruby.vim-ruby = pkgs.vimUtils.buildVimPlugin {
    name = "vim-ruby";
    src = pkgs.fetchFromGitHub {
      owner = "vim-ruby";
      repo = "vim-ruby";
      rev = "1aa8f0cd0411c093d81f4139d151f93808e53966";
      sha256 = "04ng7mjjdacajkmx20pfwlfh1h43sh6sx58id830q9jjl7kvyhhp";
    };
  };

  wellle."targets.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "targets.vim";
    src = pkgs.fetchFromGitHub {
      owner = "wellle";
      repo = "targets.vim";
      rev = "a79447f261e4b8b4327557aa03726f3849334b84";
      sha256 = "0x6a9rmv220kncjgak6aw3gbf3sidnj6nijphnsm5360lvi3ck4w";
    };
  };

  all = [
    ElmCast.elm-vim
    LnL7.vim-nix
    Raimondi.delimitMate
    farmergreg.vim-lastplace
    itchyny."lightline.vim"
    junegunn."fzf.vim"
    neoclide."coc.nvim"
    rakr.vim-one
    tpope.vim-abolish
    tpope.vim-bundler
    tpope.vim-endwise
    tpope.vim-fugitive
    tpope.vim-projectionist
    tpope.vim-rails
    tpope.vim-rake
    tpope.vim-repeat
    tpope.vim-sensible
    tpope.vim-sleuth
    tpope.vim-surround
    tpope.vim-unimpaired
    tpope.vim-vinegar
    vim-ruby.vim-ruby
    wellle."targets.vim"
  ];
}
