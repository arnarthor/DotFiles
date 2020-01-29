set updatetime=300

nmap <localleader>gg <Plug>(coc-definition)
vmap <localleader>gg <Plug>(coc-definition)
nmap <localleader>ht :call CocActionAsync('doHover')<CR>
vmap <localleader>ht :call CocActionAsync('doHover')<CR>
nmap <localleader>gi <Plug>(coc-implementation)
nmap <localleader>gr <Plug>(coc-references)

