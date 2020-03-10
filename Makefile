INSTALLER := brew

ifeq (,$(shell which scoop))
 INSTALLER := scoop
endif

.PHONY: setup
setup:
	@echo "Installing development dependencies..."
	npm i -g typescript typescript-language-server eslint
	rustup component add rls rust-analysis rust-src
	@echo "Installing the_silver_searcher..."
	$(INSTALLER) install the_silver_searcher
	@echo "Installing Wakatime..."
	pip install wakatime
