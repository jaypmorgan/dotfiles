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
Plugin 'vim-syntastic/syntastic'
Plugin 'tpope/vim-commentary'
Plugin 'challenger-deep-theme/vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

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
let g:jedi#popup_on_dot=0   " jedi settings

" COLORS
if &term =~# '^screen'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
set background=dark
set termguicolors
colorscheme challenger_deep

set number relativenumber   " hybrid numbers

" FINDING FILES
" enable fuzzy file searching from
" root project directory recursively
set path+=**
set wildmenu

" TAG JUMPING
" create the tags file
command! MakeTags !ctags -R .
command! Q :q    " always hold shift accidently when trying to leave
nnoremap <F5> :!tectonic '%:p'<CR>

" AUTOCOMPLETE
" ^n show the autocomplete list
" ^n to go the next item in the list
" ^p to go back an item in teh l^n show the autocomplete list

" SHORTCUT FOR NERDTREE
let mapleader=","
nnoremap <leader>, :NERDTree<CR>
nnoremap <leader>. :NERDTreeClose<CR>
let g:NERDTreeWinPos = "right"
" remove all trailing spaces in python
nnoremap <leader>C :%s/\s*$//g<CR>
" Vimteractive configuration
let g:vimteractive_vertical = 1  " vertically split terminal

" Syntastic Plugin Settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_python_checkers=['python', 'mypy']
let g:syntastic_python_mypy_args="--ignore-missing-import"
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_w = 1
let g:syntastic_check_on_wq = 0
set backspace=indent,eol,start

set laststatus=2 " status bar
let g:airline_theme='deus'
set ttyfast      " speed up scrolling

" display different types of whitespace
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.

" block wise movements and block text objects in julia
runtime macros/matchit.vim
