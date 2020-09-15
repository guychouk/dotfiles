INSTALLER = 
DRIVE = ~/Drive

ifeq ($(INSTALLER), brew)
 BACKUP_CMD = `brew leaves > $(DRIVE)/etc/Brewfile.txt`
 INSTALL_CMD = `brew bundle --file`
 PACKAGES = `$(DRIVE)/etc/Brewfile.txt`
endif

ifeq ($(INSTALLER), scoop)
 BACKUP_CMD = `scoop list > $(DRIVE)/etc/Scoopfile.txt`
 INSTALL_CMD = `scoop install`
 PACKAGES = `cat $(DRIVE)/etc/Scoopfile.txt | xargs`
endif

ifeq ($(INSTALLER), pacman)
 BACKUP_CMD = `pacman -Qe > $(DRIVE)/etc/Pacmanfile.txt`
 INSTALL_CMD = `pacman --noconfirm -Sy`
 PACKAGES = `cat $(DRIVE)/etc/Pacmanfile.txt | xargs`
endif

backup-pkgs::
	@echo "Backing up packages list..."
	@$(BACKUP_CMD)

install-pkgs::
	@echo "Installing packages from backup file..."
	@echo $(INSTALL_CMD) $(PACKAGES)

# For the explanation of this inline test, see:
# https://unix.stackexchange.com/a/184026/312299
init::
	@echo "Setting up gitalias..."
	@grep -q "include" ~/.gitconfig && echo "Aliases are already setup!" || echo $$'[include]\n        path = ~/.gitaliases' >> ~/.gitconfig 

bye-bye::
	@echo "Outputting dotfiles instead of deleting, change this manually to override"
	@/usr/bin/git --git-dir=$$HOME/.dotfiles/ --work-tree=$$HOME ls-files | xargs echo
