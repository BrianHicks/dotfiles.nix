{ pkgs ? import <nixpkgs> { }, ... }:

rec {

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

  airblade.vim-gitgutter = pkgs.vimUtils.buildVimPlugin {
    name = "vim-gitgutter";
    src = pkgs.fetchFromGitHub {
      owner = "airblade";
      repo = "vim-gitgutter";
      rev = "9bf988bd1d2d8001f84126d8bf74036bab33bb9b";
      sha256 = "0ksz7k29nzs00apnimp2r3hqjamdhcpr9h241hcagil3dqraqi5d";
    };
  };

  andys8.vim-elm-syntax = pkgs.vimUtils.buildVimPlugin {
    name = "vim-elm-syntax";
    src = pkgs.fetchFromGitHub {
      owner = "andys8";
      repo = "vim-elm-syntax";
      rev = "d614325a037982489574012e4db04d7f8f134c17";
      sha256 = "1wjv4z5wikh9kzgklg1b4rwsjwqnmvzppqs4hsqx3pyv8g0khdk1";
    };
  };

  autozimu.LanguageClient-neovim = pkgs.vimUtils.buildVimPlugin {
    name = "LanguageClient-neovim";
    src = pkgs.fetchFromGitHub {
      owner = "autozimu";
      repo = "LanguageClient-neovim";
      rev = "2c9c0913f16e776b40959404dcf4e23eca7a369b";
      sha256 = "0xnc2f36n9rdi5nmxnp8m2ac2za79m08a99a4ynsv1z3s4nb8087";
    };
    buildInputs = [ pkgs.curl pkgs.cacert ];
    buildPhase = "./install.sh";
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

  godlygeek.tabular = pkgs.vimUtils.buildVimPlugin {
    name = "tabular";
    src = pkgs.fetchFromGitHub {
      owner = "godlygeek";
      repo = "tabular";
      rev = "339091ac4dd1f17e225fe7d57b48aff55f99b23a";
      sha256 = "0jq91770wsp2waw7pywxjqx59v0vg54gfzirgcd85pg61w22bfla";
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

  janko.vim-test = pkgs.vimUtils.buildVimPlugin {
    name = "vim-test";
    src = pkgs.fetchFromGitHub {
      owner = "janko";
      repo = "vim-test";
      rev = "21499f536a004a98a853e54c446ba6128809fdc5";
      sha256 = "1m509p9jnb0h294vf9xjgf79gjlkxiqzx8sz75kr0msyvpnnyp6j";
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

  kkoomen.vim-doge = pkgs.vimUtils.buildVimPlugin {
    name = "vim-doge";
    src = pkgs.fetchFromGitHub {
      owner = "kkoomen";
      repo = "vim-doge";
      rev = "2fdd86981b54a57dfedba2133bd318663566b3cc";
      sha256 = "1gxpjj4hg1bl36n1fhli3dqfv0y2a1f6hfhkyh5j83gpd19wi4ny";
    };
  };

  plasticboy.vim-markdown = pkgs.vimUtils.buildVimPlugin {
    name = "vim-markdown";
    src = pkgs.fetchFromGitHub {
      owner = "plasticboy";
      repo = "vim-markdown";
      rev = "da5a7ac96f517e0fd6f886bc3fbe27156ca1f946";
      sha256 = "198bsv88njz6l2pf9yjxb8292aj7x8n2sxi50q3cdsg29a3y2i5c";
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

  sbdchd.neoformat = pkgs.vimUtils.buildVimPlugin {
    name = "neoformat";
    src = pkgs.fetchFromGitHub {
      owner = "sbdchd";
      repo = "neoformat";
      rev = "528b5e47ec9c29fbbd470f2af71b8ad994a96bdb";
      sha256 = "09cq8a4ryhxvnpjvpbm0dq22ccqfmym59avxr4c0ablx1sqy9lhy";
    };
  };

  stefandtw."quickfix-reflector.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "quickfix-reflector.vim";
    src = pkgs.fetchFromGitHub {
      owner = "stefandtw";
      repo = "quickfix-reflector.vim";
      rev = "c76b7a1f496864315eea3ff2a9d02a53128bad50";
      sha256 = "02vb7qkdprx3ksj4gwnj3j180kkdal8jky69dcjn8ivr0x8g26s8";
    };
  };

  tommcdo.vim-exchange = pkgs.vimUtils.buildVimPlugin {
    name = "vim-exchange";
    src = pkgs.fetchFromGitHub {
      owner = "tommcdo";
      repo = "vim-exchange";
      rev = "05d82b87711c6c8b9b7389bfb91c24bc4f62aa87";
      sha256 = "09fa156y8pxpzdbngifa7yzg1vjg1fjsgp1h9inj818zbig8mamb";
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

  tpope.vim-commentary = pkgs.vimUtils.buildVimPlugin {
    name = "vim-commentary";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-commentary";
      rev = "141d9d32a9fb58fe474fcc89cd7221eb2dd57b3a";
      sha256 = "0nncs32ayfhr557aiynq7b0sc7rxqwv7xanram53x1wvmfy14zf0";
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

  tpope.vim-eunuch = pkgs.vimUtils.buildVimPlugin {
    name = "vim-eunuch";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-eunuch";
      rev = "e066a0999e442d9d96f24ad9d203b1bd030ef72e";
      sha256 = "12n7fzgppiwqhqnxlbxmky1m1flb82kr4zlyggysgzz1lqb122zf";
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

  tpope.vim-rhubarb = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rhubarb";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-rhubarb";
      rev = "c509c7eedeea641f5b0bdae708581ff610fbff5b";
      sha256 = "19zhhnlrnkgsxacykck9q19rhk4gj31qjj6i4sl6bzi086kmf0z9";
    };
  };

  tpope.vim-rsi = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rsi";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-rsi";
      rev = "8b7abe2d470b7fffac6562818468e14594735564";
      sha256 = "1fvswbqd51gs4qhcabnz3zdmmlhcijs3pnbp3f2zgfi7lqvjc6s5";
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

  wellle."visual-split.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "visual-split.vim";
    src = pkgs.fetchFromGitHub {
      owner = "wellle";
      repo = "visual-split.vim";
      rev = "423a25911b3e8da04a28d29f205e1059a06e6afa";
      sha256 = "0n495ikgp10y105q9hackdk2gylnzcdadh7gflh94f3b9h0glj5z";
    };
  };

  all = [
    LnL7.vim-nix
    Raimondi.delimitMate
    airblade.vim-gitgutter
    andys8.vim-elm-syntax
    autozimu.LanguageClient-neovim
    farmergreg.vim-lastplace
    godlygeek.tabular
    itchyny."lightline.vim"
    janko.vim-test
    junegunn."fzf.vim"
    kkoomen.vim-doge
    plasticboy.vim-markdown
    rakr.vim-one
    sbdchd.neoformat
    stefandtw."quickfix-reflector.vim"
    tommcdo.vim-exchange
    tpope.vim-abolish
    tpope.vim-bundler
    tpope.vim-commentary
    tpope.vim-endwise
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
