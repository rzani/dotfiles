

"  1. Vim Plug core
"___________________________________________________


if has('vim_starting')
	set nocompatible               " Be iMproved endif
endif

" install vim
let vimplug_exists=expand('~/.vim/autoload/plug.vim') 
if !filereadable(vimplug_exists)
	echo "Installing Vim-Plug..."
	echo ""
	silent !\curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	let g:not_finish_vimplug = "yes"
	" Run shell script if exist on custom select language
	autocmd VimEnter * PlugInstall
endif

" install on neovim
let nvimplug_exists=expand('~/.local/share/nvim/site/autoload/plug.vim') 
if !filereadable(nvimplug_exists)
	echo "Installing Vim-Plug..."
	echo ""
	silent !\curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	let g:not_finish_vimplug = "yes"
	" Run shell script if exist on custom select language
	autocmd VimEnter * PlugInstall
endif




"  2. Plugins
"___________________________________________________

" Required before Plug call
call plug#begin(expand('~/.vim/plugged'))

Plug 'easymotion/vim-easymotion'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdcommenter'
Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
Plug 'sheerun/vim-polyglot'
Plug 'vim-scripts/grep.vim'
Plug 'Raimondi/delimitMate'
Plug 'editorconfig/editorconfig-vim' 
Plug 'arnaud-lb/vim-php-namespace'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'mattn/emmet-vim'
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
"Plug 'Valloric/YouCompleteMe', { 'do': './install.py --go-completer --clang-completer --js-completer' }


if has('nvim')
	Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
	Plug 'Shougo/deoplete.nvim'
	Plug 'roxma/nvim-yarp'
	Plug 'roxma/vim-hug-neovim-rpc'
endif


" Required after all Plug calls
call plug#end()



"  3. Basic Setup
"___________________________________________________


""Leader key
let mapleader=','

set autowrite

""Enconding
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8
set bomb
set binary

"" Disable showmode
set noshowmode 

"" Fix backspace indent
set backspace=indent,eol,start

"" Enable hidden buffers
set hidden

"" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

"" Improve performance
set ttyfast
set lazyredraw

"" Directories for swp files
set nobackup
set noswapfile

set fileformats=unix,dos,mac
set showcmd
set shell=/bin/zsh

" Disable visualbell
set noerrorbells visualbell t_vb=
if has('autocmd')
	autocmd GUIEnter * set visualbell t_vb=
endif

"" Copy/Paste/Cut
set clipboard=unnamedplus "share transfer area to copy/past/cut



"  4. Visual Setup
"___________________________________________________

syntax on
set ruler 
set number
set relativenumber
set wildmenu
set wildmode=longest:full,full

set background=light

let no_buffers_menu=1 

set mousemodel=popup
set t_Co=256
set guioptions=egmrti

" set cursorline

"" Disable the blinking cursor.
set gcr=a:blinkon0

" 3 bottom lines offset
set scrolloff=3

"" Status bar
set laststatus=2

"" Use modeline overrides
set modeline
set modelines=10

set title
set titleold="Terminal"
set titlestring=%F

set statusline=%F%m%r%h%w%=(%{&ff}/%Y)\ (line\ %l\/%L,\ col\ %c)\ 
if exists("*fugitive#statusline")
	set statusline+=%{fugitive#statusline()}
endif

" Auto indent
set ai

"Smart indent
set si



" 5. Plugin Settings
"___________________________________________________


"" easymotion.vim
let g:EasyMotion_smartcase=1

"" deoplete.vim
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1

" vim-airline
set laststatus=2   " Always show it
let g:airline_powerline_fonts = 1
"let g:airline_theme = 'solarized'
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tagbar#enabled = 1
let g:airline_skip_empty_sections = 1

"" NERDTree configuration
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 50
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite,*.tar.*
nnoremap <silent> <Leader>t :NERDTreeFind<CR>
noremap <F8> :NERDTreeToggle<CR>

"" grep.vim
nnoremap <silent> <leader>f :Rgrep<CR>
let Grep_Default_Options = '-IR'
let Grep_Skip_Files = '*.log *.db'
let Grep_Skip_Dirs = '.git node_modules vendor storage'


"" neosnippet
let g:neosnippet#disable_runtime_snippets = { '_' : 1, }
" Tell Neosnippet about the other snippets
let g:neosnippet#snippets_directory='~/.vim/snippets'


" 6. Functions
"___________________________________________________

function! IPhpInsertUse()
    call PhpInsertUse()
    call feedkeys('a',  'n')
endfunction



" 7. Mappings
"___________________________________________________

""General

" Make it easy save files
nmap <Leader>w :w!<cr>
inoremap jj <Esc>
inoremap jk <Esc>
inoremap kk <Esc>
inoremap <C-g> <Esc>
vnoremap <C-g> <Esc>
inoremap <C-f> <Right>
inoremap <C-b> <Left>

""Auto Complete
inoremap <C-Space> <C-x><C-o>

" Tab and Shift + Tab Circular buffer navigation
nnoremap <tab>   :bn<CR>
nnoremap <S-tab> :bp<CR>

"" Close buffer
nmap <leader>q :bp <BAR> bd #<CR>

" Close all the buffers
map <Leader>ba :bufdo bd<cr>

"" Clean search (highlight)
nnoremap <silent> <leader><space> :noh<cr>

"" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

"" Vmap for maintain Visual Mode after shifting > and <
vmap < <gv
vmap > >gv
vmap <Leader><Space> <Esc>

" Moving line up and down
nnoremap <C-Up> :m .-2<CR>==
nnoremap <C-Down> :m .+1<CR>==
inoremap <C-Up> <Esc>:m .-2<CR>==gi
inoremap <C-Down> <Esc>:m .+1<CR>==gi
vnoremap <A-Up> :m '<-2<CR>gv=gv''
vnoremap <C-Down> :m '>+1<CR>gv=gv

"" easymotion.vim
map <Space> <Plug>(easymotion-prefix)
map <Space>s <Plug>(easymotion-overwin-f)
map <Space><Space> <Plug>(easymotion-sn)
omap <Space><Space> <Plug>(easymotion-tn)

"" Git
noremap <Leader>ga :Gwrite<CR>
noremap <Leader>gc :Gcommit<CR>
noremap <Leader>gp :Gpush<CR>
noremap <Leader>gl :Gpull<CR>
noremap <Leader>gs :Gstatus<CR>
noremap <Leader>gbh :Gblame<CR>
noremap <Leader>gd :Gvdiff<CR>
noremap <Leader>gr :Gremove<CR>

"" CtrlP
cnoremap <C-E> <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <Leader>r :CtrlPMRUFiles<cr>
nnoremap <C-P> :CtrlP<cr>
"noremap <C-B> :CtrlPBuffer<CR>
noremap <Leader><Tab> :CtrlPBuffer<CR>

"" Golang
autocmd FileType go nmap <leader>b  <Plug>(go-build)
autocmd FileType go nmap <leader>r  <Plug>(go-run)
autocmd FileType go nmap <leader>t  <Plug>(go-test)

"" PHP
autocmd FileType php inoremap <Leader>u <Esc>:call IPhpInsertUse()<CR>
autocmd FileType php noremap <Leader>u :call PhpInsertUse()<CR>

"" Emmet
autocmd FileType blade imap <Tab> <C-y>,

"" Editing files
" Make it easy to edit the Vimrc file
nmap <Leader>ev :e $MYVIMRC<cr>

"" Open file explorer
map <Leader>e :e<Space><C-d>

"" neosnippet
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"


" 8. Autocmd Rules
"___________________________________________________


" Automatically source the Vimrc file on save
"
augroup autosourcing
	autocmd!
	autocmd BufWritePost .vimrc source %
	autocmd BufWritePost init.vim source %
augroup END

"" Remember cursor position
augroup vimrc-remember-cursor-position
	autocmd!
	autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

" NERDTress File highlighting
function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
 exec 'autocmd filetype nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
 exec 'autocmd filetype nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

call NERDTreeHighlightFile('jade', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ini', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#151515')
call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('styl', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('coffee', 'Red', 'none', 'red', '#151515')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')
call NERDTreeHighlightFile('php', 'Magenta', 'none', '#ff00ff', '#151515')


