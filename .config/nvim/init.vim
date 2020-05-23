""""""""""""""""""""""""""""""""""""""
"        ________        ________
"       /        \      /        \
"       \        /      \        /
"        |      |        /     /'
"        |      |      /     /'
"        |      |    /     /' 
"        |      |  /     /'     
"        |      |/   ___          
"        |          /  /  _ _ _ _
"        |         ___  / _   _  \
"        |       //  / / / / / / /
"        |     /' / / / / / / / /
"        |   /'  / / / / / / / /
"        |_/'  /__//__//__//___/
"                   
"""""""""""""""""""""""""""""""""""""

let mapleader =" "			" Leader como espacio

"""""""""""""""""""
" Plugins
"""""""""""""""""""
call plug#begin()
Plug 'junegunn/goyo.vim'
Plug 'chrisbra/Colorizer'
Plug 'SidOfc/mkdx'
Plug 'dhruvasagar/vim-table-mode'
Plug 'godlygeek/tabular'
Plug 'tpope/vim-eunuch'
Plug 'junegunn/limelight.vim'
call plug#end()

"""""""""""""""""""
" UI
"""""""""""""""""""
set number relativenumber      " Números de lineas relativos
set ruler		       " Lugar en el documento en la parte inferior de la pantalla
set showcmd		       " Mostrar comandos en la parte inferior de la pantalla
set incsearch		       " Busqueda "en vivo" de palabras con /
set hlsearch		       " Resaltar resultados de busquedas
set wildmenu		       " es a dos puntos lo que counsel es a M-x
set wildmode=list:longest,full
"set noshowmode		       " Quitar el texto debajo de modeline
set linebreak		       " Corta palabras al final de la misma
set laststatus=0	       " Activa el modeline
set textwidth=0		       " Desactiva el hard linebreak
set formatoptions+=t
set rulerformat=%18(%l,%c\ %m%r%)\ %P
syntax enable
filetype plugin indent on

let g:lightline = {
      \ 'colorscheme': 'gruvbox',
      \ }

set cursorline			" activa el resaltado de la linea
highlight CursorLine cterm=NONE ctermbg=black

"""""""""""""""""
" Funcionamiento
"""""""""""""""""

set autoread			" Actualizar buffers cuando son editados fuera de vim
au FocusGained,BufEnter * checktime
set ignorecase		        " Ignorar mayúsculas cuando busca
set autoindent		        " Sangría automática cuando se crea una nueva debajo de otra ya indentada
set clipboard=unnamedplus       " Portapapeles del sistema activado
set encoding=utf-8	        " Codificación utf8
set smartcase		        " Mayúsculas automáticas inteligentes cuando busca
set shiftwidth=4	        " TAB mide 4 espacios (no estoy seguro)
set softtabstop=4	        " TAB mide 4 espacios (no estoy seguro)
set smartindent		        " Sangría automática en lineas nuevas cerradas por llaves
set smarttab		        " No se, pero sirve para borrar tabs
set virtualedit=block	        " Cursor libre cuando se usa visualblock
set backspace=eol,start,indent  " Backspace funciona bien con tabs
set whichwrap+=<,>,h,l	        " h y j respeta tabs
set undofile
set undodir=~/.config/nvim/undodir
autocmd FileType markdown TableModeEnable
set mouse=a


"""""""""""""
" Maping
"""""""""""""

" Moverse con jk en lugar de gj y gk 
nnoremap <expr> j (v:count > 4 ? "m'" . v:count . 'j' : 'gj')
nnoremap <expr> k (v:count > 4 ? "m'" . v:count . 'k' : 'gk')

nnoremap <leader><tab> /<++><CR>cw

"Activar Goyo con F6
noremap <F6> :Goyo <CR>
"highlight CursorLine cterm=NONE ctermbg=black <CR>
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight! | highlight CursorLine cterm=NONE ctermbg=black

autocmd  User GoyoLeave "Activar spellcheck con F5
noremap <F5> :setlocal spell! spelllang=es<CR>

" Abrir con space space
noremap <leader><leader> :Ntree<CR>

" Teclas desactivadas
"""""""""""""""""""""
noremap q: <Nop>
nnoremap Q <Nop>


autocmd FileType markdown inoremap ses<tab> <esc>:read ~/.config/nvim/snips/sesion<CR>kddA 
autocmd FileType markdown inoremap sec<tab> <esc>:read ~/.config/nvim/snips/secuencia<CR>kdd2wcw 
autocmd FileType markdown inoremap sec<tab> <esc>:read ~/.config/nvim/snips/secuencia<CR>kdd2wcw 

command D :Rename DONE_%:t|wq




" Color name (:help cterm-colors) or ANSI code
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
let g:limelight_default_coefficient = 0.9
autocmd InsertEnter * norm zz
