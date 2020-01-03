set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'JuliaEditorSupport/julia-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-scripts/indentpython.vim'
Plugin 'sheerun/vim-polyglot' " syntax highlighting
Plugin 'williamjameshandley/vimteractive'

call vundle#end()             " required
filetype plugin indent on     " required
syntax on

" python features
set cc=100
set encoding=utf-8
set modeline
set tabstop=4 
set expandtab 
set shiftwidth=4 
set softtabstop=4
set cursorline
set showmatch
let python_highlight_all=1

" COLORS
set background=dark
set number relativenumber   " hybrid numbers


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
let g:NERDTreeWinPos = "right"

" Vimteractive configuration
let g:vimteractive_vertical = 1  " vertically split terminal
set backspace=indent,eol,start
