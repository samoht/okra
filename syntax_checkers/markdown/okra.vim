if exists('g:loaded_syntastic_markdown_okra')
    finish
endif
let g:loaded_syntastic_markdown_okra = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_markdown_okra_GetLocList() dict
    let makeprg = self.makeprgBuild({'args': ['lint', '--short']})

    let errorformat = '%f:%l:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1]})
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'markdown',
    \ 'name': 'okra'})

let &cpo = s:save_cpo
unlet s:save_cpo
