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
Plug 'itchyny/lightline.vim'
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/goyo.vim'
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
set noshowmode		       " Quitar el texto debajo de modeline
set linebreak		       " Corta palabras al final de la misma
set laststatus=2
set textwidth=0
set formatoptions+=t
syntax enable
filetype plugin indent on

let g:lightline = {
      \ 'colorscheme': 'minimal',
      \ }

set cursorline
highlight CursorLine cterm=NONE ctermbg=16

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

nnoremap <expr> j (v:count > 4 ? "m'" . v:count . 'j' : 'gj')
nnoremap <expr> k (v:count > 4 ? "m'" . v:count . 'k' : 'gk')
