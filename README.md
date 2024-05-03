# $HOME sweet $HOME

This repo is used to store the configuration files of all of the software I use on a daily basis.

Each program's directory essentially mirrors my `$HOME` directory. This is so I can install the configuration of a chosen program by using [GNU Stow](https://www.gnu.org/software/stow/).

Here's an example of how I'd use it to restore my vim configuration on a fresh machine:

```sh
cd ~
git clone https://github.com/guychouk/dotfiles
cd dotfiles
stow vim
```

That's it really, from here on out I try to go over what my work environment setup is if you're interested.

## macOS Setup

I do most of my work on macOS, so there's a specific `osxsetup` script in the `home/scripts` directory.

There's also a `Brewfile` with a list of all of the programs I need, which I can reinstall on a fresh machine using:

```shell
cd ~/dotfiles
brew bundle install
```

## Shell - ZSH

My `.zshrc` is pretty short and well documented, so I'll only discuss some of the tools I use to make it a bit more usable:

* [FASD](https://github.com/clvv/fasd) for better navigation in the terminal
* [FZF](https://github.com/junegunn/fzf) for pretty much anything
* [direnv](https://direnv.net/) for setting up environment variables in projects
* [asdf](https://github.com/asdf-vm/asdf) for managing runtime versions
* Basic time functions: `countdown` and `stopwatch`
* Other commands and aliases (mostly `git` helpers using `fzf` for listing)

## Terminal - kitty + tmux

I found [kitty](https://sw.kovidgoyal.net/kitty/) to be awesome, with only one config file that's easy to backup. It has everything a modern (lol) terminal should have and I find it to be very light.

After setting up the shell, we need to review a few givens:
* We need to have a way to split the current window of the terminal we're working on
* We need to have multiple windows and an easy way to move between open sessions
* We need it to be robust so if the terminal closes, the sessions stay open

This can all be achieved by using [tmux](https://github.com/tmux/tmux/wiki).

## Editor - Vim

Vim to me is a text editor and an interface to my work environment.

The rest of how I use it can only be described by my `vimrc`, but some noteable things would be:

* Filetype specifics configuration in the `after/ftplugin` directory, and filetype specific compiler setup in the `compiler` directory
* Some custom filetype setup in the `ftdetect` directory
* Some syntax highlighting for some filetypes in the `syntax` directory
* Various helper functions divided to namespaces written in Vim9 scripts
    * quickfix helpers
    * The zoompane helper
    * Other custom functionality divided by plugin
    * To use these helpers, I can call them from the main `vimrc` (e.g. `:call zoompane#Toggle()`)

For the plugins, I use vim's built in plugin system (`:h packages`) and a custom `setup.sh` script to install all of the packages in the `packages.txt` file.

I also wrote a very minimal `statusline` "plugin" of my own called `microline`.

### Universal Ctags

My [ctags](https://github.com/universal-ctags/ctags) configuration includes some directories to exclude by default and some better TypeScript regex definitions. I dislike language servers, so I mostly get by on `ctags -R .` and use the generated tags to navigate or autocomplete from vim.

If there are local changes I want to make in the way the tags are generated, I can create a `.gutctags` file in the root of the project with specific rules to extract tags, and it's all thanks to the excellent [vim-gutentags](https://github.com/ludovicchabant/vim-gutentags) plugin. Here's an example of such a file to parse YAML OpenAPI files and generate tags for each operation:

```text
--recurse=yes

--exclude=.git
--exclude=api-types
--exclude=node_modules
--exclude=bundled-public-apis
--exclude=bundled-internal-apis

--languages=YAML
--regex-YAML=/^[ \t]*['"]?\/([^:]+)['"]?:.*$/\1/s,section/
--regex-YAML=/operationId:[ \t]*['"]?([^'"\n]+)['"]?$/\1/o,operation/
```

## Snippets

To handle snippets, I use the `home/scripts/snip` script to parse the `home/snippets.txt` file, pipe them to `fzf` for easy search and when a snippet is selected, it pipes it to vim for further editing if needed. When done, it copies the edited snippet so it's available in my clipboard.

## Window Tiling

I'm not a fan of "spaces" or whatchamacallit on macOS, I prefer my ⌘-Tab to move between windows, an old bad habit from my Windows days I guess (what can I say I miss Alt-Tab). Since I like to work on only one desktop, I use [spectacle](https://github.com/eczarny/spectacle) for window tiling.

## That's it really

This pretty much concludes the most critical pieces of software, the rest are my configurations for programs such as `curl`, `git`, [`yazi`](https://github.com/sxyazi/yazi) which is a terminal file manager, and various others.

There's also my `home/scripts` directory with various documented scripts to check out.

Thanks for sticking with me this far! I hope you find something useful in this repo. And if not, well here's a consolation cookie 🍪.
