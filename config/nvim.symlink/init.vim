" init.vim/.vimrc

" Os/Variant specifc stuff {{
  let s:is_win = has('win64') || has('win32')
  let s:is_nix = has('unix') || has('macunix') || has('win32unix')
  let s:is_unix = has('unix')
  let s:is_mac = has('macunix')
  let s:is_gui = has('gui_running')
  let s:mac_gui = has('gui_macvim') && has('gui_running')
  let s:is_nvim = has('nvim')

  if s:is_win
    if s:is_nvim
      let s:vim_root='~\AppData\Local\nvim\'
    else
      let s:vim_root='~\vimfiles\'
    endif
    let s:vim_esc='\'
  elseif s:is_nix
    if s:is_nvim
      let s:vim_root='~/.config/nvim/'
    else
      let s:vim_root='~/.vim/'
    endif
    let s:vim_esc='/'
  endif
  " let s:vim_tmp_dir=s:vim_root.".tmp".s:vim_esc
  let s:vim_tmp_dir='~/.vim-tmp/'
" }}

" Environ vars {{
  let $NVIM_COC_LOG_FILE=s:vim_tmp_dir.'coc.log'
  let $NVIM_COC_LOG_LEVEL='off'
" }}

" Plugins {{
  " Ensure plug is installed
  call functions#PlugCheck()

  " Plug as plugin manager
  call plug#begin(s:vim_root . 'plugged')

  " Themes
  Plug 'morhetz/gruvbox'
  Plug 'tomasr/molokai'
  Plug 'joshdick/onedark.vim'
  " Airline + AirlineThemes
  " Plug 'vim-airline/vim-airline'
  " Plug 'vim-airline/vim-airline-themes'
  " LightLine
  Plug 'itchyny/lightline.vim'
  Plug 'kien/rainbow_parentheses.vim'
  " Nerdtree
  Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
  " NerdCommenter
  Plug 'preservim/nerdcommenter'
  " Completions
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'editorconfig/editorconfig-vim'
  " Syntax Highlight
  Plug 'sheerun/vim-polyglot'
  " Icons for nerdtree and ctrlp
  " Plug 'ryanoasis/vim-devicons'
  Plug 'tpope/vim-surround'

  call plug#end()
" }}


" General Settings {{

  " Be vImproved
  set nocompatible

  " Syntax on always
  syntax on                       " Highlight syntax

  " What kind of file
  filetype plugin indent on

  " Theme {{
    set background=dark
    if has('termguicolors') | set termguicolors | endif
    silent! colorscheme gruvbox
    " silent! colorscheme molokai
    " silent! colorscheme onedark
  " }}

  " Gui Font {{
    let s:myFont='Hack'
    let s:myFontSize = 12
    function! SetFontSize(size)
      let s:myFontSize = a:size
      " if s:is_nvim
        " execute 'GuiFont! '.s:myFont.':h'.s:myFontSize
      " else
        silent execute 'set guifont='.s:myFont.':h'.s:myFontSize
      " endif
    endfunction
    call SetFontSize(s:myFontSize)
  " }}


  " Basic config {{
    set encoding=UTF-8              " Set encoding to UTF-8
    set laststatus=2                " Always display the status line
    set showcmd
    set ruler
    set noerrorbells
    set modelines=0
    set nomodeline
    set nu                          " Show numbers
    set rnu                         " Show relative numbers
    set splitbelow                  " Split below for new horizontal split
    set splitright                  " Split right for new vertical split
    set autoindent                  " Auto Indent
    set smartindent                 " Smart Indent
    set autoread                    " Auto read files
    set fileformats=unix,mac,dos    " Handle line endings prefer LF
    set wildignorecase              " Case-insensitive completions
    set wildmode=list:longest,full  " Show list of completions and complete as much as possible, then iterate full completions
    set infercase                   " Adjust completions to match case
    set updatetime=500              " Idleness is 2sec
    set undofile                    " Enable undo file
    " Set keycodes/maps timeout 300ms
    set timeout
    set timeoutlen=300
    set ttimeoutlen=300
    set undolevels=100              " Undo Levels
    set clipboard+=unnamedplus
    set noshowmode
    set nolazyredraw                " don't redraw while executing macros
    set magic                       " Set magic on, for regex

    "==[ Vim Info ]==
    "           +--Disable hlsearch while loading viminfo
    "           | +--Remember marks for last 500 files
    "           | |    +--Remember up to 1000 lines in each register
    "           | |    |     +--Remember up to 1MB in each register
    "           | |    |     |     +--Remember last 200 search patterns
    "           | |    |     |     |    +---Remember last 100 commands
    "           | |    |     |     |    |
    "           v v    v     v     v    v
    set viminfo=h,'500,<1000,s1024,/200,:100
  " }}


  " Smart Search {{
    set incsearch                  " do incremental searchinq
    set ignorecase                 " ignore case serch
    set smartcase                  " unless Upper letters are used
    set hls                        " highlight all searchs
    " highlight clear Search
    " highlight       Search  cterm=bold ctermfg=Black  ctermbg=Yellow
    " highlight    IncSearch  cterm=bold ctermfg=White  ctermbg=Red
  " }}


  " Whitespace settings {{
    " set noexpandtab                " Do not expand tabs
    set smarttab                   " Smart tabs
    " set tabstop=4                  " global tabstop 4spaces will be changed based on filetype below
    " set shiftwidth=4
    " set softtabstop=4

    " set list                       " Show Whitespaces
    " set listchars=tab:→\ ,space:·,nbsp:.,extends:❯,precedes:❮
    set listchars=tab:→\ ,nbsp:.,extends:❯,precedes:❮
    map <F3> :set list! list? <CR>

    " Show trailing white space as Red blocks
    highlight TrailingWhiteSpace ctermbg=red guibg=red
    match TrailingWhiteSpace /\s\+$/
  " }}

  " Cursor Line {{
    set cursorline
    set colorcolumn=80
    " highlight ColorColumn ctermbg=lightgrey guibg=lightgrey
  " }}

  " Code folding settings {{
    set foldmethod=syntax             " fold based on indent
    set foldlevelstart=99
    set foldnestmax=10                " deepest fold is 10 levels
    set nofoldenable                  " don't fold by default
    set foldlevel=1
  " }}

  " Backup/Swap/Undo stuff {{
    " set nobackup
    " set noswapfile
    execute 'set backupdir='.s:vim_tmp_dir.'backup'.s:vim_esc.s:vim_esc
    execute 'set directory='.s:vim_tmp_dir.'swap'.s:vim_esc.s:vim_esc
    execute 'set undodir='.s:vim_tmp_dir.'undo'.s:vim_esc.s:vim_esc
    set backupskip=/tmp/*,/private/tmp/*"
  " }}


  " Remember last line
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"zz" | endif

  " Comments shall look italic and LightGreen
  highlight Comment term=bold cterm=italic ctermfg=LightGreen gui=italic guifg=LightGreen

  " AutoReload vimrc after save
  augroup VimReload
    autocmd!
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
  augroup END

  " Removes pipes | that act as seperators on vertical splits
  " set fillchars+=vert:\

  " Term Colors {{
    set t_Co=256
    if (has("termguicolors"))
      set termguicolors
    endif
  " }}
" }}

" Key Bindings {{

  " Leader
  let mapleader = ","

  " Quick Access to command mode
  nnoremap ; :
  xnoremap ; :

  " Quick exit insert mode
  inoremap jk <Esc>

  " Quick Save
  nnoremap <leader>, :w<CR>
  " Quick Exit
  nnoremap <leader>q :q<CR>

  nmap <leader>l :set list! list?<CR>

  " Buffer movement {{
    " Switch between current and last buffer
    nnoremap <leader>. <C-^>
    nnoremap <leader>n :bn<CR>
    nnoremap <leader>p :bp<CR>
  " }}

  " Window movement {{
    nmap <silent> <C-h> :wincmd h<CR>
    nmap <silent> <C-j> :wincmd j<CR>
    nmap <silent> <C-k> :wincmd k<CR>
    nmap <silent> <C-l> :wincmd l<CR>
  " }}

  " Move across visual line under wrap
  nnoremap j gj
  nnoremap k gk
  nnoremap <Down> gj
  nnoremap <Up> gk
  nnoremap <silent> ^ g^
  nnoremap <silent> $ g$
  " Slipts Resizing
  map - <C-W>-
  map + <C-W>+
" }}


" Plugin Settings {{

  " Vim Lightline {{
    let g:lightline = {
      \   'colorscheme' : 'one',
      \ }
  " }}

  " Vim Airline {{
    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
  " }}

  " RainBow parentheses Always On {{
    augroup RainBowAUGrp
      au!
      au VimEnter * RainbowParenthesesToggle
      au Syntax * RainbowParenthesesLoadRound
      au Syntax * RainbowParenthesesLoadSquare
      au Syntax * RainbowParenthesesLoadBraces
    augroup END
  " }}

  " CoC {{
    " GoTo code navigation.
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gy <Plug>(coc-type-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)
    nmap <silent> gh <Plug>(coc-dohover)
    " Formatting selected code.
    xmap <leader>f  <Plug>(coc-format-selected)
    nmap <leader>f  <Plug>(coc-format-selected)
    " Symbol renaming.
    nmap <leader>rn <Plug>(coc-rename)
    " Use <tab> for trigger completion and navigate to the next complete item
    function! s:check_back_space() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction
    inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
    inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"

    " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
    " position. Coc only does snippet and additional edit on confirm.
    if exists('*complete_info')
      inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
    else
      imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
    endif
  " }}

  " NerdTree {{
    map <silent> <C-n> :NERDTreeToggle<CR>
    let NERDTreeMinimalUI = 1
    let g:NERDTreeDirArrowExpandable = '►'
    let g:NERDTreeDirArrowCollapsible = '▼'
    " let g:NERDTreeWinSize=38
    " let NERDTreeDirArrowExpandable = "\u00a0" " make arrows invisible
    " let NERDTreeDirArrowCollapsible = "\u00a0" " make arrows invisible
    highlight NERDTreeClosable ctermfg=2
    highlight NERDTreeOpenable ctermfg=8

    augroup nerdtree
      au!
      au FileType nerdtree setlocal nolist " turn off whitespace characters
      au FileType nerdtree setlocal nocursorline " turn off line highlighting for performance
      " Open NerdTree when folder is provided as argument
      au StdinReadPre * let s:std_in=1
      " autocmd VimEnter * if argc() == 0 && !exists("s:std_in") && v:this_session == "" | NERDTree | endif
      au VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
    augroup END
  " }}

  " NerdCommenter {{
    map <silent> <C-/> <Plug>NERDCommenterToggle
    " Windows sends Ctrl+_ instead of Ctrl+/
    map <silent> <C-_> <Plug>NERDCommenterToggle
    let g:NERDSpaceDelims = 1
    let g:NERDCompactSexyComs = 1
    let g:NERDCommentEmptyLines = 1
    let g:NERDTrimTrailingWhitespace = 1
  " }}

  " CtrlP {{
    let g:ctrlp_working_path_mode = 'ra'
    let g:ctrlp_root_markers = ['configure.ac', '.gitignore']
    let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
    if executable('rg')
      let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
    endif
  " }}
" }}


" Filetype specifc stupidities {{

  "==[ File Type detection ]==
  augroup FileTypeDetect
    au!
    au BufRead,BufNewFile .*rc*       setfiletype anyrc
    au BufRead,BufNewFile Android.bp  setfiletype BluePrint
    au BufRead,BufNewFile *.mk        setfiletype make
    au BufRead,BufNewFile *.cc        setfiletype ccCpp
  augroup END

  " Default WhiteSpace settings
  augroup FiletypeDefaultWS
    au!
    au Filetype * setlocal ts=4 sw=4 sts=4 noexpandtab
  augroup END

  " FileType Specific WhiteSpace overrides
  augroup FileTypeSpecifcWS
    au!
    au Filetype anyrc setlocal ts=2 sw=2 expandtab
    au Filetype vim setlocal ts=2 sw=2 expandtab
    au Filetype html setlocal ts=2 sw=2 expandtab
    au Filetype xml setlocal ts=2 sw=2 expandtab
    au Filetype ccCpp setlocal ts=2 sw=2 sts=0 expandtab
    au Filetype javascript setlocal ts=2 sw=2 sts=0 expandtab
    au Filetype java setlocal ts=4 sw=4 sts=0 expandtab
    au Filetype BluePrint setlocal ts=4 sw=4 sts=0 expandtab
  augroup END
" }}

