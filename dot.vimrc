"set nobackup
set backspace=indent,eol,start
set nostartofline
set ruler
set showcmd
set showmatch
set showmode
set tabstop=4
set shiftwidth=4
set viminfo=
set background=light
set mouse=
set number
syntax on

" Syntax colors.
highlight Todo ctermbg=Red ctermfg=Yellow
" Set up the status line so it's colored and always on. Very jedish.
set laststatus=2
highlight StatusLine cterm=none ctermbg=DarkBlue ctermfg=white
highlight StatusLineNC cterm=none ctermbg=DarkBlue ctermfg=white
highlight VertSplit cterm=none ctermbg=DarkBlue ctermfg=white
set et

au FileType perl,c,java,cpp,yaml,python,ruby set smartindent
au FileType cpp set sw=2 ts=2
au FileType make set sw=8 ts=8 autoindent noet
au FileType tex set ts=78 ts=2 sw=2 smartindent et
au FileType ruby set ts=2 sw=2

augroup Mail
    au!
    au FileType mail set tw=68 fc=tcqr2 nomodeline et
    au FileType mail set comments+=n:\|
augroup END

filetype plugin indent on

silent! call pathogen#infect()
