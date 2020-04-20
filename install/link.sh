#!/usr/bin/env bash

if [ "$(readlink -f $0)" = "$(readlink -f $BASH_SOURCE)" ]; then
  DOTFILES=$(readlink -f $(dirname $0)/../)
  source $DOTFILES/install/colors
fi

if [ ! -d "$HOME/.config" ]; then
  echo "Creating ~/.config"
  mkdir -p "$HOME/.config"
fi

# $1 file
# $2 target
check_create_symlink()
{
  local file=$1
  local target=$2
  # echo "check_create_symlink $file -> $target"
  if [ -e "$target" ]; then
    echo -e "~${target#$HOME} ${BG_G}already exists...${BG_NC} Skipping."
    return 0
  elif [ -L $target -a ! -e $target ]; then
    echo -e "${R}Broken symlink will delete..${NC}"
    rm -f $target
  fi
  echo -e "${G}Creating symlink for${NC} ~${target#$HOME}"
  ln -s $file $target
}

echo -e "\n${Cy}Creating symlinks${NC}"
echo -e "${Cy}------------------------------${NC}"
linkables=$(find -H "$DOTFILES" -maxdepth 3 -name '*.symlink' -printf '%P ')
for file in $linkables ; do
  target=$HOME/.${file%.symlink}
  check_create_symlink $DOTFILES/$file $target
done

echo -e "\n\n${Cy}Creating vim symlinks${NC}"
echo -e "${Cy}------------------------------${NC}"
check_create_symlink $DOTFILES/config/nvim.symlink $HOME/.vim
check_create_symlink $DOTFILES/config/nvim.symlink/init.vim $HOME/.vimrc

