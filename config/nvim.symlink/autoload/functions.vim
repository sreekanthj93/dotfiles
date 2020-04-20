let s:is_win = has('win64') || has('win32')
let s:is_nix = has('unix') || has('macunix') || has('win32unix')

let s:vim_autoload = expand('<sfile>:p:h')

" Download vim-plug if it is not present
let s:vim_plugpath = s:vim_autoload . '/plug.vim' " this is relative to this file, which is in autoload
function! functions#PlugCheck()
  if !filereadable(s:vim_plugpath)
    if s:is_win
      silent execute '!powershell md ' . s:vim_autoload
      silent execute '!powershell Invoke-WebRequest -Uri https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim -OutFile ' . s:vim_plugpath
    elseif s:is_nix
      silent execute '!curl -fLo ' . s:vim_plugpath . ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    endif
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif
endfunction
