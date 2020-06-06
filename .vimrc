" VIMRC Config File
" author: Jay Morgan

set nocompatible              " be iMproved, required
filetype off                  " required

" execute local vimrc files
set exrc
set secure

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'JuliaEditorSupport/julia-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-scripts/indentpython.vim'
Plugin 'sheerun/vim-polyglot' " syntax highlighting
Plugin 'vim-syntastic/syntastic'
Plugin 'tpope/vim-commentary'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'vhdirk/vim-cmake'
Plugin 'Yggdroot/indentLine'
Plugin 'derekwyatt/vim-fswitch'
Plugin 'jiangmiao/auto-pairs'
Plugin 'majutsushi/tagbar'
Plugin 'xavierd/clang_complete'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'chriskempson/base16-vim'
Plugin 'vimwiki/vimwiki'
Plugin 'kassio/neoterm'
Plugin 'jlanzarotta/bufexplorer'
Plugin 'neoclide/coc.nvim', {'branch': 'release'}
Plugin 'metakirby5/codi.vim'
Plugin 'mbbill/undotree'
Plugin 'jremmen/vim-ripgrep'
Plugin 'junegunn/fzf'

call vundle#end()             " required
filetype plugin indent on     " required
syntax on

set cc=100
set encoding=utf-8
set modeline
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smartcase
set cursorline
hi CursorLine term=bold cterm=bold guibg=Grey30
set showmatch
set number
set splitbelow
set splitright
set nohlsearch
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set incsearch


" Relative numbers on active buffer, else absolute
autocmd BufEnter,FocusGained,InsertLeave  * set relativenumber
autocmd BufLeave,FocusLost,InsertEnter    * set norelativenumber

" COLORS
if &term =~# '^screen'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
set termguicolors
let base16colorspace=256
set background=light
colorscheme base16-default-dark
let g:airline_theme='base16_default'

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
command! Black !black %

""""""""""""""""""""""""""""
"        SHORTCUTS         "
""""""""""""""""""""""""""""

"" Change the leader to a comma
let mapleader=" "

"" NerdTree
nnoremap ,, :NERDTreeToggle<CR>
let g:NERDTreeWinPos = "right"

"" Misc
" remove all trailing spaces
nnoremap ,C :%s/\s*$//g<CR>
nnoremap <leader>ss :w<CR>
function OpenSmallTerminal()
    if has('nvim')
        :tnoremap <Esc> <C-\><C-n>
        :below 10sp term://bash
        :execute "normal $i"
    else
        :bel term
        :resize 20
    endif
    :setlocal nonu
endfunction
nnoremap <silent> <leader>ot :call OpenSmallTerminal()<CR>

function OpenScratchBuffer()
    :new
    :res 10
    :setlocal buftype=nofile
    :setlocal bufhidden=hide
    :setlocal noswapfile
endfunction
nnoremap <silent> <leader>os :call OpenScratchBuffer()<CR>
nnoremap <F5> :!clear && tectonic '%:p'<CR>

function Send2REPL(mode)
    if len(keys(g:neoterm.instances)) == 0
        :Topen
    endif
    if a:mode ==# 'v'
        :TREPLSendSelection
    else
        :TREPLSendLine
    endif
    :normal $
    :call search("\\S$", "W")
endfunction
nnoremap <silent> <leader>c :silent call Send2REPL('n')<CR>
xnoremap <silent> <leader>c :silent call TREPLSendSelection<CR>
vnoremap <silent> <leader>c :<C-U>TREPLSendSelection<CR>

"" vim-fswitch
nnoremap <leader>sw :FSHere<CR>

"" tagbar
nnoremap <leader>o; :TagbarToggle<CR>

" Terminal remap for neovim
tnoremap <Esc> <C-\><C-n>

" Window management
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>

" Undo Tree
nnoremap <leader>u :UndotreeToggle<CR>

" RipGrep
nnoremap <leader>r :Rg 

" Fuzzy File Search
nnoremap <leader>p :FZF<CR>

nnoremap <silent> <leader>ow :VimwikiIndex<CR>

" Split buffers
nnoremap <silent> <leader>sv :split new<CR>
nnoremap <silent> <leader>sh :vsplit new<CR>

"""""""""""""""""""""""""""
"     PLUGIN SETTINGS     "
"""""""""""""""""""""""""""
" python settings
let python_highlight_all=1
let g:jedi#auto_initialization=1
let g:jedi#show_call_signatures=0
let g:pymode_folding=0
let g:pymode_rope=0
let g:jedi#completions_enabled=0      " use deoplete-vim as its async in neovim
let g:deoplete#sources#jedi#show_docstring=1

au VimEnter,BufRead,BufNewFile *.tex set textwidth=75
au VimEnter,BufRead,BufNewFile *.tex set formatoptions+=a
au VimEnter,BufRead,BufNewFile *.tex set wrap

" Syntastic Plugin Settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_python_checkers=['python', 'mypy']
let g:syntastic_python_mypy_args="--ignore-missing-import"
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_w = 1
let g:syntastic_check_on_wq = 0
set backspace=indent,eol,start

set laststatus=2 " status bar
set ttyfast      " speed up scrolling

" display different types of whitespace
set list
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.

" block wise movements and block text objects in julia
runtime macros/matchit.vim

" deoplete.nvim
let g:python_host_prog="/usr/bin/python2"
let g:python3_host_prog="/usr/bin/python3"

let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

au VimEnter,BufRead,BufNewFile *.jl set filetype=julia
au VimEnter,BufRead,BufNewFile *.idr set filetype=idris
au VimEnter,BufRead,BufNewFile *.lidr set filetype=lidris
au VimEnter,BufRead,BufNewFile *.lfe set filetype=lfe

if executable('rg')
    let g:rg_derive_root='true'
endif

let g:latex_to_unicode_tab = 0
let g:latex_to_unicode_suggestions = 0

" Coc
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" TextEdit might fail if hidden is not set.
set hidden
" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup
" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300
" Don't pass messages to |ins-completion-menu|.
set shortmess+=c
" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()
