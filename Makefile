.PHONY: update-doc test-zsh-keybindings

update-doc:
	@echo "Copying keybinding documentation..."
	@mkdir -p docs/keybinding
	@cp -r ~/.cache/nixos-config/keybinding/* docs/keybinding/
	@echo "Done! Files copied to docs/keybinding/"

test-zsh-keybindings:
	@zsh packages/my-generate-zsh-keybindings/my-generate-zsh-keybindings.zsh --test
