.PHONY: update-doc

update-doc:
	@echo "Copying keybinding documentation..."
	@mkdir -p docs/keybinding
	@cp -r ~/.cache/nixos-config/keybinding/* docs/keybinding/
	@echo "Done! Files copied to docs/keybinding/"
