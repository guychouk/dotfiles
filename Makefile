INSTALLER = 
INSTALL_CMD = 
DRIVE = /mnt/d/Drive

ifeq ($(INSTALLER), cask)
 INSTALL_CMD = `brew cask install`
endif

ifeq ($(INSTALLER), brew)
 INSTALL_CMD = `brew install`
endif

ifeq ($(INSTALLER), scoop)
 INSTALL_CMD = `scoop install`
endif

ifeq ($(INSTALLER), pacman)
 INSTALL_CMD = `pacman --noconfirm -Sy`
endif

# Use only if not setting an installer will break something
#ifeq ($(INSTALLER),)
# $(error Forgot to set INSTALLER)
#endif

PACKAGES_LIST = `cat $(DRIVE)/etc/$(INSTALLER)-list.txt | xargs`

.PHONY: install
install:
	@echo $(INSTALLER) $(PACKAGES_LIST)

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
