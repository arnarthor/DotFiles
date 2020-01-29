syntax on
set background=dark
colorscheme onedark

set undofile
set undodir=~/.config/nvim/undo
set history=10000
set hidden
set notimeout
set number
set numberwidth=4
set relativenumber
set wrap
set autoindent
set lcs=trail:·,tab:»·
set nolist
set autoread
set backspace=eol,start,indent
set encoding=utf-8
set fileencoding=utf-8
set termguicolors
set mouse=a
set lazyredraw
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set showmatch
set scrolloff=5
set hlsearch
set ignorecase
set smartcase
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
set wildignore+=*/node_modules/*
set wildmenu
"set shiftround
"set smarttab

"" Configure NERDTree
let NERDTreeIgnore = ['\.pyc$']
" NERDTree configuration
" Disable nerdtree for opeing up in new sessions.
let g:NERDTreeHijackNetrw=0

"" Configure Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='deus'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#formatter = 'default'
let g:airline_powerline_fonts = 0
let g:airline_left_sep = ''
let g:airline_right_sep = ''
"let g:airline_theme='ayu'

