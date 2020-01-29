set completeopt+=menuone
set completeopt+=noselect
set completeopt+=noinsert
set completeopt-=preview

let g:mucomplete#enable_auto_at_startup = 1
let g:mucomplete#no_mappings = 1
let g:mucomplete#chains = {'default': ['omni']}

let g:mucomplete#can_complete = {}
let g:mucomplete#chains = {
  \ 'default' : ['path', 'omni'],
  \ 'vim'     : ['path', 'cmd', 'ulti', 'keyn']
  \ }

" Have to disable merlin mappings.
let g:merlin_disable_default_keybindings=1
exec "xnoremap <silent> . <s-tab> :call UltiSnips#SaveLastVisualSelection()<cr>gvs"
