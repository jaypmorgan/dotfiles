" VIMRC Config File
" author: Jay Morgan

set nocompatible              " be iMproved, required
filetype off                  " required

" execute local vimrc files
set exrc
set secure

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/YouCompleteMe
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
Plugin 'airblade/vim-gitgutter'
Plugin 'vhdirk/vim-cmake'
Plugin 'Yggdroot/indentLine'
Plugin 'derekwyatt/vim-fswitch'
Plugin 'jiangmiao/auto-pairs'
Plugin 'jceb/vim-orgmode'
Plugin 'tpope/vim-speeddating' " required for org-mode editing
Plugin 'colepeters/spacemacs-theme.vim'
Plugin 'sonph/onehalf', {'rtp': 'vim/'}
Plugin 'majutsushi/tagbar'
Plugin 'xavierd/clang_complete'
Plugin 'Shougo/deoplete.nvim'

call vundle#end()             " required
filetype plugin indent on     " required
syntax on

set cc=100
set encoding=utf-8
set modeline
set tabstop=4
set expandtab
set shiftwidth=4
set softtabstop=4
set cursorline
hi CursorLine term=bold cterm=bold guibg=Grey30
set showmatch
set number relativenumber   " hybrid numbers

" COLORS
if &term =~# '^screen'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif
set background=dark
colorscheme onehalfdark
let g:airline_theme='onehalfdark'

" FINDING FILES
" enable fuzzy file searching from
" root project directory recursively
set path+=**
set wildmenu

""""""""""""""""""""""""""""
"        COMMANDS          "
""""""""""""""""""""""""""""
" create the tags file
command! MakeTags !ctags -R .
command! Q :q    " always hold shift accidently when trying to leave
command! W :w    " when accidently holding shift
command! EC :e ~/.vimrc

""""""""""""""""""""""""""""
"        SHORTCUTS         "
""""""""""""""""""""""""""""

"" Change the leader to a comma
let mapleader=","

"" NerdTree
nnoremap <leader>, :NERDTreeToggle<CR>
let g:NERDTreeWinPos = "right"

"" Misc
" remove all trailing spaces
nnoremap <leader>C :%s/\s*$//g<CR>
nnoremap <leader>w :w<CR>
function OpenSmallTerminal()
    :bel terminal
    :resize 20
endfunction
nnoremap <leader>t :call OpenSmallTerminal()<CR>
nnoremap <F5> :!clear && tectonic '%:p'<CR>

"" vim-fswitch
nnoremap <leader>sw :FSHere<CR>

"" tagbar
nnoremap <leader>; :TagbarToggle<CR>

"""""""""""""""""""""""""""
"     PLUGIN SETTINGS     "
"""""""""""""""""""""""""""

" python settings
let python_highlight_all=1
let g:jedi#popup_on_dot=0

" Vimteractive configuration
let g:vimteractive_vertical = 1  " vertically split terminal

" " YouCompleteMe
" hi Pmenu ctermbg=gray guibg=gray
" let g:ycm_confirm_extra_conf=0
" let g:ycm_show_diagnostics_ui=0
" let g:ycm_server_python_interpreter='/usr/bin/python3'
" let g:ycm_python_binary_path='/usr/bin/python3'

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
" let g:airline_theme='deus'
set ttyfast      " speed up scrolling

" display different types of whitespace
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.

" block wise movements and block text objects in julia
runtime macros/matchit.vim

" deoplete.nvim
let g:deoplete#sources#clang#libclang_path='/usr/lib/llvm-6.0/lib/libclang.so'
let g:deoplete#sources#clang#clang_header='/usr/lib/llvm-6.0/lib/clang/'
