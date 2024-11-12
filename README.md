# $HOME sweet $HOME 🏠

This repo is used to store the configuration files of all of the software I use on a daily basis.

Every directory in this repo is a configuration directory for a specific piece of software, and I use my own [`link.sh`](./link.sh) script to symlink the configuration files as stated in the [`LINKS`](./LINKS) file.

Here's an example of how I'd use it to restore my vim configuration on a fresh machine:

```sh
cd ~
git clone https://github.com/guychouk/dotfiles
cd dotfiles
./link.sh
```

From here on out, I go over what my work environment setup is if you're interested.

## MacOS

I do most of my work on macOS, so that's why there's a specific `osxsetup` script in the [`scripts`](./scripts) directory.

There's also a `Brewfile` with a list of all of my bare essentials, which I can reinstall on a fresh machine using:

```shell
cd ~/dotfiles
brew bundle install
```

I'm not a fan of "spaces" or whatchamacallits on macOS, I prefer my ⌘-Tab to move between windows, an old bad habit from my Windows days I guess (what can I say I miss Alt-Tab). Since I like to work on only one desktop, I use [Rectangle](https://rectangleapp.com/) for window tiling.

## ZSH

My `.zshrc` is pretty short and well documented, so I'll only discuss some of the tools I use to make it a bit more usable:

* [zoxide](https://github.com/ajeetdsouza/zoxide) for better terminal navigation (can't live without `j`)
* [fzf](https://github.com/junegunn/fzf) for pretty much anything that involves filtering
* [direnv](https://direnv.net/) for setting up environment variables in projects
* [asdf](https://github.com/asdf-vm/asdf) for managing runtime versions
* [zsh-abbr](https://zsh-abbr.olets.dev/) for abbreviations
* Basic time functions: `countdown` and `stopwatch`
* Git helpers using `fzf` for listing branches, commits, and tags

### Snippets

I tend to forget a lot, so I rely on my own solution for snippets.

In my scripts directory I have the `snip` script which parses the [`SNIPPETS`](./SNIPPETS) file, pipes them to `fzf` for easy search and when a snippet is selected, it pipes it to vim for further editing if needed. When done, it copies the edited snippet so it's available in my clipboard.

## kitty

I found [kitty](https://sw.kovidgoyal.net/kitty/) to be awesome, with only one config file that's easy to backup.

It has everything a modern terminal should have and I find it to be very light and stable.

After setting up the shell, we need to review a few givens:
* We need to have a way to split the current window of the terminal we're working on
* We need to have multiple windows and an easy way to move between open sessions

This can all be achieved by using kitty as well (see my config for more information).

Previously I used `tmux` as a terminal multiplexer, but after following [Kovid Goyal](https://www.kovidgoyal.net/) (creator of the kitty terminal) for quite some time, he convinced me to drop `tmux` in favor of kitty, and the switch was so easy that I never looked back.

Since I work exclusively locally and don't require session management, I really don't see any reason to use `tmux`, and removing one extra moving part from my setup sounded too good to pass.

## Vim

Too much has already been said on Vim that I don't think I can add anything new to the conversation.

What I _can_ say is that I've used both Emacs AND Vim [for quite some time now](https://github.com/guychouk/dotfiles/commit/e53aa5c7ee1c796ce78e74b49a8cb6c185c633c6), and lord knows I've tried so many distros and plugins over the years, but what _really_ stuck is the most minimal Vim setup possible.

That's because I now see Vim for what it truely is: an *interface* to my work environment, _not_ a tool that should have everything but the kitchen sink (I'm looking at you emacs).

My point is that these days I never see the need to add a plugin to Vim to do something that I can do with a simple shell command, a script or a full blown program (which I can always *call* from Vim!).

Keeping it light is a true delight, so I recommend you take a look at my `vimrc` to see how I've set it up. How I use Vim can only be really described by my `vimrc`, but here are some highlights:

* Compiler setup in the `compiler` directory
* My own colorscheme called [`busy-g`](./vim/colors/busy-g.vim)
* A custom `statusline` setup using my own plugin called [`picoline`](./vim/pack/personal/start/picoline).
* I use Vim's built in plugin system (`:h packages`) and a custom [`pack.sh`](./vim/pack.sh) script to install all of the packages in the [`PACKAGES`](./vim/PACKAGES) file.

### Universal Ctags

My [ctags](https://github.com/universal-ctags/ctags) configuration includes some directories to exclude by default and some better TypeScript regex definitions. I'm not fond of language servers, so I mostly get by on `ctags -R .` and use the generated tags to navigate or autocomplete from vim.

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

## That's it really

This pretty much concludes the most critical pieces of software, the rest are my configurations for programs such as `curl`, `git`, [`yazi`](https://github.com/sxyazi/yazi), and various others.

There's also my `home/scripts` directory with various documented scripts to check out.

Thanks for sticking around! I hope you find something useful in this repo. And if not, well, here's a consolation cookie 🍪.
