INSTALLER = 
DRIVE = ~/Drive

ifeq ($(INSTALLER), brew)
 INSTALL_CMD = `brew bundle --file`
 PACKAGES = `$(DRIVE)/etc/Brewfile`
endif

ifeq ($(INSTALLER), scoop)
 INSTALL_CMD = `scoop install`
 PACKAGES = `cat $(DRIVE)/etc/$(INSTALLER)-list.txt | xargs`
endif

ifeq ($(INSTALLER), pacman)
 INSTALL_CMD = `pacman --noconfirm -Sy`
 PACKAGES = `cat $(DRIVE)/etc/$(INSTALLER)-list.txt | xargs`
endif

.PHONY: install
install:
	@echo $(INSTALL_CMD) $(PACKAGES)

.PHONY: setup
setup:
	@echo "Symlinking Espanso config dir..."
	ln -s $(DRIVE)/etc/.espanso ~/Library/Preferences/espanso
	@echo "Symlinking SSH config dir..."
	ln -s $(DRIVE)/etc/.ssh ~/.ssh

# Easy way to run conditional statements in a make file where using
# an if statement would be problematic due to how make runs commands.
# See https://unix.stackexchange.com/a/184026/312299
.PHONY: gitalias
gitalias:
	@grep -q "include" ~/.gitconfig && echo "Aliases are already setup!" || echo $$'[include]\n        path = ~/.gitaliases' >> ~/.gitconfig 
