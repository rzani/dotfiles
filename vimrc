

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

Plug 'mhinz/vim-startify'
Plug 'easymotion/vim-easymotion'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'scrooloose/nerdcommenter'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'sheerun/vim-polyglot'
Plug 'vim-scripts/grep.vim'
Plug 'jiangmiao/auto-pairs'
" Plug 'editorconfig/editorconfig-vim'
" Plug 'sgur/vim-editorconfig'
Plug 'editorconfig/editorconfig-vim'
Plug 'arnaud-lb/vim-php-namespace'
Plug 'Xuyuanp/nerdtree-git-plugin'
" Plug 'mattn/emmet-vim'
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'
Plug 'w0rp/ale'
Plug 'majutsushi/tagbar'
Plug 'terryma/vim-multiple-cursors'
Plug 'posva/vim-vue'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'zchee/deoplete-go', { 'do': 'make'}

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

"" Enabling mouse
set mouse=a

set autowrite

""Enconding
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8

set bomb
set binary

" A buffer becomes hidden when it is abandoned
set hid

"" Disable showmode
set noshowmode

"" Fix backspace indent
set bs=2
" set backspace=indent,eol,start

"" Enable hidden buffers
set hidden

"" Default open buffers below
set splitbelow

"" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

"" Improve performance
set ttyfast
set lazyredraw

"" Directories for swp files
set backupcopy=yes "https://en.parceljs.org/hmr.html#safe-write
set nobackup
set noswapfile
set undofile
set undodir=/tmp

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
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite,*.tar.*

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
set completeopt+=noselect
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']

"" Airline
set laststatus=2   " Always show it
let g:airline_powerline_fonts = 1
let g:airline_theme = 'angr'
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tagbar#enabled = 1
let g:airline_skip_empty_sections = 1
let g:airline#extensions#tabline#left_alt_sep = ' '
let g:airline_right_sep='│'
let g:airline_left_sep='│'

"" grep.vim
nnoremap <silent> <leader>f :Rgrep<CR>
let Grep_Default_Options = '-IR'
let Grep_Skip_Files = '*.log *.db'
let Grep_Skip_Dirs = '.git node_modules vendor storage'

"" neosnippet
let g:neosnippet#disable_runtime_snippets = { '_' : 1, }
" Tell Neosnippet about the other snippets
let g:neosnippet#snippets_directory='~/.vim/snippets'
" Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1

"" CtrlP - Ignore files
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'

"" The Silver Searcher
if executable('ag')
	" Use ag over grep
	set grepprg=ag\ --nogroup\ --nocolor

	" Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
	let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

	" " ag is fast enough that CtrlP doesn't need to cache
	let g:ctrlp_use_caching = 0
endif


"" netrw rules!
"
"  http://vimcasts.org/episodes/the-file-explorer/
"
" Bindings
" 	- = open netrw and after opened go to up dir
" 	% = new file
"   	d = create new directory
"   	R = rename file or directories
"   	D = deleting files or directories
"
let g:netrw_liststyle = 1 " use `i`  to change this value
let g:netrw_browse_split = 0 " open file in previous window
let g:netrw_winsize = 25
let g:netrw_altv = 1

"" Startify
let g:startify_lists = [
			\ { 'type': 'sessions',  'header': [   'Sessions']       },
			\ { 'type': 'files',     'header': [   'MRU']            },
			\ { 'type': 'dir',       'header': [   'MRU '. getcwd()] },
			\ { 'type': 'bookmarks', 'header': [   'Bookmarks']      },
			\ { 'type': 'commands',  'header': [   'Commands']       },
			\ ]



" 6. Functions
"___________________________________________________

function! IPhpInsertUse()
	call PhpInsertUse()
	call feedkeys('a',  'n')
endfunction


" 7. Mappings
"___________________________________________________

""General

"" Make it easy save files
nmap <Leader>w :w!<cr>
inoremap jj <Esc>
inoremap jk <Esc>
inoremap kk <Esc>
inoremap <C-g> <Esc>
vnoremap <C-g> <Esc>
inoremap <C-f> <Right>
inoremap <C-b> <Left>

"" netrw
map - :Explore<CR>

nnoremap <C-j> i<CR><Esc>

" Terminal Mode
"
" Open terminal
" 	:terminal
" 	sp | terminal
"
" Escape to normal mode
" 	<C-\><C-n>
"
" Open file under the cursor
" 	gf
"
if has('nvim')
	tnoremap <C-[> <C-\><C-n>
	" Terminal mode:
	tnoremap <M-h> <c-\><c-n><c-w>h
	tnoremap <M-j> <c-\><c-n><c-w>j
	tnoremap <M-k> <c-\><c-n><c-w>k
	tnoremap <M-l> <c-\><c-n><c-w>l
	" Insert mode:
	inoremap <M-h> <Esc><c-w>h
	inoremap <M-j> <Esc><c-w>j
	inoremap <M-k> <Esc><c-w>k
	inoremap <M-l> <Esc><c-w>l
	" Visual mode:
	vnoremap <M-h> <Esc><c-w>h
	vnoremap <M-j> <Esc><c-w>j
	vnoremap <M-k> <Esc><c-w>k
	vnoremap <M-l> <Esc><c-w>l
	" Normal mode:
	nnoremap <M-h> <c-w>h
	nnoremap <M-j> <c-w>j
	nnoremap <M-k> <c-w>k
	nnoremap <M-l> <c-w>l
endif


""Auto Complete
inoremap <C-Space> <C-x><C-o>

"" Tab and Shift + Tab Circular buffer navigation
nnoremap <tab>   :bn<CR>
nnoremap <S-tab> :bp<CR>
nnoremap <Leader>. <C-^><CR>

"" Close buffer
nmap <leader>q :bp <BAR> bd! #<CR>

"" Close all the buffers
map <Leader>ba :bufdo bd<cr>

"" Clean search (highlight)
nnoremap <silent> <leader><space> :noh<cr>

"" Set working directory
nnoremap <leader>, :lcd %:p:h<CR>

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

" session management
nnoremap <leader>so :SLoad<Space>
nnoremap <leader>ss :SSave<Space>
nnoremap <leader>sd :SDelete<CR>
nnoremap <leader>sc :SClose<CR>

"" Golang
autocmd FileType go nmap <leader>b  <Plug>(go-build)
autocmd FileType go nmap <leader>r  <Plug>(go-run)
autocmd FileType go nmap <leader>t  <Plug>(go-test)

"" PHP
autocmd FileType php inoremap <Leader>u <Esc>:call IPhpInsertUse()<CR>
autocmd FileType php noremap <Leader>u :call PhpInsertUse()<CR>

"" Emmet
autocmd FileType blade imap <C-j> <C-y>,
autocmd FileType vue imap <C-j> <C-y>,

"" Editing files
" Make it easy to edit the Vimrc file
nmap <Leader>vi :e $MYVIMRC<cr>

"" Open file explorer
map <Leader>e :Explore<cr>

"" neosnippet
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" Indent the whole buffer
map <Leader>i mzgg=G`z

" open Tagbar
map <F8> :TagbarOpenAutoClose<CR>
map <F5> :e<CR>



" 8. Autocmd Rules
"___________________________________________________

" Automatically source the Vimrc file on save
"
augroup autosourcing
	autocmd!
	if has('nvim')
		autocmd BufWritePost init.vim source %
	else
		autocmd BufWritePost .vimrc source %
	endif
augroup END

"" Remember cursor position
augroup vimrc-remember-cursor-position
	autocmd!
	autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END
