let g:mapleader = "\<Space>"
let g:maplocalleader = ','

" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

nnoremap <silent> <leader><space> :WhichKey '<Space>'<CR>
nnoremap <silent> <localleader>, :WhichKey ','<CR>
nnoremap <esc> :noh<CR>

inoremap jj <Esc>
nnoremap <leader>qq :qa<CR>
nnoremap <leader>cl :Commentary<CR>

" NERDTree
nnoremap <leader>ftt :NERDTreeToggle<CR>
nnoremap <leader>ftf :NERDTreeFocus<CR>
" /NERDTree

" FILES
nnoremap <leader>fs :w<CR>
" /FILES

" FUGITIVE
nnoremap <leader>gs :Gstatus<CR>
" /FUGITIVE

" FZF
nnoremap <leader>ph :GFiles<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>saf :Rg<CR>
" /FZF

" BUFFERS
nnoremap <leader>bd :bd<CR>
nnoremap <leader>bn :bn<CR>
nnoremap <leader><Tab> :b#<CR>
" /BUFFERS

" VIM CONFIG
nnoremap <leader>fed :exe 'edit '.stdpath('config').'/init.vim'<CR>
nnoremap <leader>fer :source ~/.config/nvim/init.vim<CR>
" /VIM CONFIG

" MUNDO
nnoremap <leader>au :MundoShow<CR>
" /MUNDO
