#!/usr/bin/env bash

DOTFILES=$(readlink -f $(dirname $0))
source $DOTFILES/install/colors

echo -e "\n${LM}Installing dotfiles...${NC}"

source $DOTFILES/install/link.sh

echo -e "\nCreating vim/nvim tmp directories"
mkdir -p $HOME/.vim-tmp/backup $HOME/.vim-tmp/swap $HOME/.vim-tmp/undo
mkdir -p $HOME/.nvim-tmp/backup $HOME/.nvim-tmp/swap $HOME/.nvim-tmp/undo

echo -e "\n\n${BG_G}Done..${BG_NC} Reload your terminal...\n"
