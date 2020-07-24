# load all aliases from .alias.d folder
for aliasfile in ~/.alias.d/*
  source $aliasfile
end

# load custom alias here if present
test -s ~/.alias && source ~/.alias || true

# load custom fish config
test -s ~/.fish_my && source ~/.fish_my || true

# ~/local/bin usages
if test -d "$HOME/local/bin"
  set PATH $HOME/local/bin $PATH
end
# ~/local/usr/bin usages
if test -d "$HOME/local/usr/bin"
  set PATH $HOME/local/usr/bin $PATH
end
# ~/.local/bin usages
if test -d "$HOME/.local/bin"
  set PATH $HOME/.local/bin $PATH
end
# ~/.local/usr/bin usages
if test -d "$HOME/.local/usr/bin"
  set PATH $HOME/.local/usr/bin $PATH
end
