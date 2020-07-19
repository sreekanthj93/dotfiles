# load all aliases from .alias.d folder
for aliasfile in ~/.alias.d/*
    source $aliasfile
end

# load custom alias here if present
test -s ~/.alias && source ~/.alias || true

# ~/local/bin usages
if test -d "$HOME/local/bin"
    export PATH=$HOME/local/bin:$PATH
end
# ~/local/usr/bin usages
if test -d "$HOME/local/usr/bin"
    export PATH=$HOME/local/usr/bin:$PATH
end
# ~/.local/bin usages
if test -d "$HOME/.local/bin"
    export PATH=$HOME/.local/bin:$PATH
end