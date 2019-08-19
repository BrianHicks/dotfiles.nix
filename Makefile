result: dotfiles/neovim/plugins.nix $(shell find dotfiles darwin -type f) darwin.sh
	./darwin.sh build

dotfiles/neovim/plugins.nix: dotfiles/neovim/plugins.json dotfiles/neovim/plugins.py
	nix-shell -p python37 --run "python dotfiles/neovim/plugins.py $< > $@"
	./nixfmt.sh $@
