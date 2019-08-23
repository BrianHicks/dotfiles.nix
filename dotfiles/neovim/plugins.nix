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

  Shougo.neco-syntax = pkgs.vimUtils.buildVimPlugin {
    name = "neco-syntax";
    src = pkgs.fetchFromGitHub {
      owner = "Shougo";
      repo = "neco-syntax";
      rev = "98cba4a98a4f44dcff80216d0b4aa6f41c2ce3e3";
      sha256 = "1cjcbgx3h00g91ifgw30q5n97x4nprsr4kwirydws79fcs4vkgip";
    };
  };

  airblade.vim-gitgutter = pkgs.vimUtils.buildVimPlugin {
    name = "vim-gitgutter";
    src = pkgs.fetchFromGitHub {
      owner = "airblade";
      repo = "vim-gitgutter";
      rev = "e929cb8b868aba97331231ece6f9f3e1204babea";
      sha256 = "0naf2pyjcylqgrw7j6nvl6kb4g2gc5936621w957qhj98c3cp8fx";
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

  dense-analysis.ale = pkgs.vimUtils.buildVimPlugin {
    name = "ale";
    src = pkgs.fetchFromGitHub {
      owner = "dense-analysis";
      repo = "ale";
      rev = "73812c3e41c1c7fcf1705811f35ac4c9ccec003e";
      sha256 = "166hgzyx1j1n717icj0mq2n8jkg4kpi1iy5gk3q0l28nd88w5hlb";
    };
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

  fgrsnau.ncm2-otherbuf = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-otherbuf";
    src = pkgs.fetchFromGitHub {
      owner = "fgrsnau";
      repo = "ncm2-otherbuf";
      rev = "8625b68044db9e2a2b896952c2226ee11918ace4";
      sha256 = "0sir3a6vwyci5ynxii6d1mxli05cwz5z4j8rqflm3sb645c7vjsv";
    };
  };

  glts.vim-magnum = pkgs.vimUtils.buildVimPlugin {
    name = "vim-magnum";
    src = pkgs.fetchFromGitHub {
      owner = "glts";
      repo = "vim-magnum";
      rev = "2c9c3e35322ed3607528629ae561f19d6933e66a";
      sha256 = "14aq35p2sx068isfp0wkv81kbghwfy4gh1r0y6bdwki3zxbxpf1b";
    };
  };

  glts.vim-radical = pkgs.vimUtils.buildVimPlugin {
    name = "vim-radical";
    src = pkgs.fetchFromGitHub {
      owner = "glts";
      repo = "vim-radical";
      rev = "d3f1d33c53c8b1b378b5c1ef3e078452025f46b0";
      sha256 = "0qjjis28142035djrn2ahm4dfxgrxi7gs6r19za009hmllbamag8";
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
      rev = "8b3aa1632c08784928458b2b85faf5c89feefccf";
      sha256 = "0bxvxkk0zps2qxnzp8dip7ngpv9b1a74y2wjzjqqvxzljk81a714";
    };
  };

  janko.vim-test = pkgs.vimUtils.buildVimPlugin {
    name = "vim-test";
    src = pkgs.fetchFromGitHub {
      owner = "janko";
      repo = "vim-test";
      rev = "4ab87cfc827ddf3b98b18eee352ca4b4edc56363";
      sha256 = "1jxacncn8iklx93fw0732bqpr0p51hlw9f4fjv3jd0l2qkjv0jp7";
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

  kassio.neoterm = pkgs.vimUtils.buildVimPlugin {
    name = "neoterm";
    src = pkgs.fetchFromGitHub {
      owner = "kassio";
      repo = "neoterm";
      rev = "c96ff95c9a41d716bbdca41b4cc2dc62e4b188a1";
      sha256 = "0jbrgsdljh9n1cr3dpgq83gpd3hx7ss4hbc0i0hls15laczlzipc";
    };
  };

  kkoomen.vim-doge = pkgs.vimUtils.buildVimPlugin {
    name = "vim-doge";
    src = pkgs.fetchFromGitHub {
      owner = "kkoomen";
      repo = "vim-doge";
      rev = "bd49e4abb30cfd0f75e656d6e6056e1e8d05c63e";
      sha256 = "1gxpjj4hg1bl36n1fhli3dqfv0y2a1f6hfhkyh5j83gpd19wi4ny";
    };
  };

  maximbaz.lightline-ale = pkgs.vimUtils.buildVimPlugin {
    name = "lightline-ale";
    src = pkgs.fetchFromGitHub {
      owner = "maximbaz";
      repo = "lightline-ale";
      rev = "dd59077f9537b344f7ae80f713c1e4856ec1520c";
      sha256 = "1f9v6nsksy36s5i27nfx6vmyfyjk27p2w2g6x25cw56b0r3sgxmx";
    };
  };

  mileszs."ack.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "ack.vim";
    src = pkgs.fetchFromGitHub {
      owner = "mileszs";
      repo = "ack.vim";
      rev = "36e40f9ec91bdbf6f1adf408522a73a6925c3042";
      sha256 = "0yppr89hd1jyp0pj56hxdjbn32sr7pj3mihd18wxispvl5dqd6fm";
    };
  };

  ncm2.ncm2 = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2";
    src = pkgs.fetchFromGitHub {
      owner = "ncm2";
      repo = "ncm2";
      rev = "53b6531769e43c7e3c9051e3a12ab31e3e06a422";
      sha256 = "1kf2gfcw0wmyib72na3j2dsw6q4qff1r9lvdbk7cm7iclhwylhma";
    };
  };

  ncm2.ncm2-bufword = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-bufword";
    src = pkgs.fetchFromGitHub {
      owner = "ncm2";
      repo = "ncm2-bufword";
      rev = "1d42750114e47a31286268880affcd66c6ae48d5";
      sha256 = "14q76n5c70wvi48wm1alyckba71rp5300i35091ga197nkgphyaz";
    };
  };

  ncm2.ncm2-path = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-path";
    src = pkgs.fetchFromGitHub {
      owner = "ncm2";
      repo = "ncm2-path";
      rev = "84b1e6b5f28ced2245ff08e6694101f029fdfca8";
      sha256 = "0yqga8d423k2j6iknkyx1qs1shddpshi4sx78992sa15dax9d394";
    };
  };

  ncm2.ncm2-syntax = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-syntax";
    src = pkgs.fetchFromGitHub {
      owner = "ncm2";
      repo = "ncm2-syntax";
      rev = "7cd3857001a219be4bc7593b7378034b462415e4";
      sha256 = "0l36qvsclhg8vr1ix1kpdl0kh739gp6b7s03f18vf9f0aj0im6w2";
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

  roxma.nvim-yarp = pkgs.vimUtils.buildVimPlugin {
    name = "nvim-yarp";
    src = pkgs.fetchFromGitHub {
      owner = "roxma";
      repo = "nvim-yarp";
      rev = "8fcb1af27772174df5446d49de29052cac47e46f";
      sha256 = "0ya3xgbnpps6s67rxfwpcfv39micl1d2wblzb7xvs1pmsymwbj0r";
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

  svermeulen.ncm2-yoink = pkgs.vimUtils.buildVimPlugin {
    name = "ncm2-yoink";
    src = pkgs.fetchFromGitHub {
      owner = "svermeulen";
      repo = "ncm2-yoink";
      rev = "802070a996527c4ee227287fc2cdf1f5a8f5d4f2";
      sha256 = "10lzw3xmxcjk9iwii0xbik8y4cmd0bl3r7kc3xcdvs4mzqpnbypa";
    };
  };

  svermeulen.vim-subversive = pkgs.vimUtils.buildVimPlugin {
    name = "vim-subversive";
    src = pkgs.fetchFromGitHub {
      owner = "svermeulen";
      repo = "vim-subversive";
      rev = "5837cb38f656f120e7a04ae73f749303d78b9191";
      sha256 = "0n04mxdq80xkjgi75n1c2gg2s6am5kns8rj7pz6dvvlqr4vxyrjf";
    };
  };

  svermeulen.vim-yoink = pkgs.vimUtils.buildVimPlugin {
    name = "vim-yoink";
    src = pkgs.fetchFromGitHub {
      owner = "svermeulen";
      repo = "vim-yoink";
      rev = "e6748f69de44fb043d41237e57c1e3883e524aef";
      sha256 = "1msf9iqm4hnqymkl5rm3ild2a2lpz7cwwjav8jqqv95jpz9pq5kn";
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

  tpope.vim-dispatch = pkgs.vimUtils.buildVimPlugin {
    name = "vim-dispatch";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-dispatch";
      rev = "a76bec9196fe27e195d167a5c2ee1da763d31b96";
      sha256 = "0a9sxpdpll68drk3w98xvmv2z31q4afw70iwjrb7lmp8raxn0i2z";
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
      rev = "f04a2275dbe525df5e0a4dcdfb368d768850f8dd";
      sha256 = "10fxg6d91mj4i1657pgm42pj3fc9yxjqy9vnyl6dl6ga7xg1qy4z";
    };
  };

  tpope.vim-projectionist = pkgs.vimUtils.buildVimPlugin {
    name = "vim-projectionist";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-projectionist";
      rev = "b1a826329c0891416f2357bf77a43ac49b441e6d";
      sha256 = "0za2hnsg888nl3ddhawll053j64sgqhiqpxciqi05j50bz34cs8n";
    };
  };

  tpope.vim-rails = pkgs.vimUtils.buildVimPlugin {
    name = "vim-rails";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-rails";
      rev = "e191b246e2475b26e07e7b18928a80735c31ffa9";
      sha256 = "03l895d2a226i8k1n0p8fxw685mxfgip5daxihnp3i2cw0kn432m";
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
      rev = "9edacf9d5b4d6e0570af33f88500f51ec4288c2e";
      sha256 = "0m91nvxjkgmbgaib3q27rk2nzkpxx18pa8nrv143r2k8na9bry0p";
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
