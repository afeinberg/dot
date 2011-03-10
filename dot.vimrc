set nobackup
set backspace=indent,eol,start
set nostartofline
set ruler
set showcmd
set showmatch
set showmode
set tabstop=4
set shiftwidth=4
set viminfo=
"set background=dark
syntax on

au FileType perl,c,java,cpp,yaml,python,ruby set smartindent et
au FileType make set sw=8 ts=8 autoindent
au FileType tex set ts=78 ts=2 sw=2 smartindent et
au FileType ruby set ts=2 sw=2

augroup Mail
    au!
    au FileType mail set tw=68 fc=tcqr2 nomodeline et
    au FileType mail set comments+=n:\|
augroup END
