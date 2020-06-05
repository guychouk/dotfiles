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

ifeq ($(INSTALLER),)
 $(error Forgot to set INSTALLER)
endif

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

.PHONY: gitalias
gitalias:
	@-grep -q "include" ~/.gitconfig
	@if [ $$? = 1 ]; then \
	  @echo $$'[include]\n        path = ~/.gitaliases' >> ~/.gitconfig; \
	fi
