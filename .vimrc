set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'JuliaEditorSupport/julia-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-scripts/indentpython.vim'

call vundle#end()            " required
filetype plugin indent on    " required
syntax on

" python features
set encoding=utf-8
set modeline
set tabstop=4 
set expandtab 
set shiftwidth=4 
set softtabstop=4
set cursorline
set showmatch
let python_highlight_all=1

set background=dark
set relativenumber

" FINDING FILES
" enable fuzzy file searching from
" root project directory recursively
set path+=**
set wildmenu

" TAG JUMPING
" create the tags file
command! MakeTags !ctags -R .

" AUTOCOMPLETE
" ^n show the autocomplete list
" ^n to go the next item in the list
" ^p to go back an item in teh l^n show the autocomplete list

" SHORTCUT FOR NERDTREE
let mapleader=","
:nnoremap <leader>n :NERDTree<cr>
