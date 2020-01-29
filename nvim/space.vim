call which_key#register('<Space>', "g:which_key_map")
call which_key#register(',', "g:which_key_map_local")

let g:which_key_map_local =  {}

let g:which_key_map =  {}

let g:which_key_map.b = {
       \ 'name' : '+buffer' ,
       \ 'd' : 'delete-buffer',
       \ 'f' : 'first-buffer',
       \ 'l' : 'last-buffer',
       \ 'n' : 'next-buffer',
       \ 'p' : 'previous-buffer',
       \ 'b' : 'fzf-buffer',
       \ 'x' : 'delete-buffer-and-window',
       \ }

let g:which_key_map['c'] = {
      \ 'name' : '+comments' ,
      \ 'l' : 'comment-or-uncomment-lines',
      \ 'p' : 'comment-or-uncomment-paragraph',
      \ }

let g:which_key_map['g'] = {
      \ 'name' : '+git/version-control' ,
      \ 'b' : 'fugitive-blame',
      \ 'c' : 'commits-for-curent-buffer',
      \ 'C' : 'fugitive-commit',
      \ 'd' : 'fugitive-diff',
      \ 'e' : 'fugitive-edit',
      \ 'l' : 'fugitive-log',
      \ 'm' : 'fugitive-move',
      \ 'r' : 'fugitive-read',
      \ 's' : 'fugitive-status',
      \ 'w' : 'fugitive-write',
      \ 'p' : 'fugitive-pull',
      \ 'P' : 'fugitive-push',
      \ }

let g:which_key_map.w = { 'name' : '+window' }
let g:which_key_map.w.s = { 'name' : '+split' }

let g:which_key_map.f = { 'name' : '+file' }
let g:which_key_map.f.t = { 'name' : '+tree' }
let g:which_key_map.f.t.t = 'toggle'
let g:which_key_map.f.t.f = 'find'
let g:which_key_map.f.e = { 'name' : '+environment' }
let g:which_key_map.f.e.e = 'vimrc'
let g:which_key_map.f.e.r = 'reload'
let g:which_key_map.p = { 'name' : '+ctrlp' }
let g:which_key_map.p.b = "buffers"
let g:which_key_map.p.f = "files"
"call which_key#register('<Space>', "g:which_key_map")

" Configuration ctrlP
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|\.git$\'
