# $HOME sweet $HOME

This is a repository for my dotfiles and other configuration files. I use [GNU Stow](https://www.gnu.org/software/stow/) to manage them.

After cloning the repository, you can use the following commands to install the configuration files for a specific program:

```sh
cd ~
git clone <this-repo>
cd <this-repo>
stow <program>
```
