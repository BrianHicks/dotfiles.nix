dotfiles/neovim/plugins.nix: dotfiles/neovim/plugins.json dotfiles/neovim/plugins.py
	nix-shell -p python37 --run "python dotfiles/neovim/plugins.py $< > $@"
	./nixfmt.sh $@
