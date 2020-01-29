let g:python_host_prog = "/usr/local/bin/python"
let g:python3_host_prog = "/usr/local/bin/python3"

call plug#begin()

" Plug 'jordwalke/vim-reasonml'
Plug 'reasonml-editor/vim-reason-plus'
Plug 'Shougo/neco-vim'
Plug 'neoclide/coc-neco'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'tpope/vim-fugitive'
Plug 'joshdick/onedark.vim'
Plug 'tpope/vim-commentary'
Plug 'vim-airline/vim-airline-themes'
Plug 'liuchengxu/vim-which-key'
Plug 'simnalamburt/vim-mundo'
Plug 'tpope/vim-surround'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'cohama/lexima.vim'

call plug#end()
