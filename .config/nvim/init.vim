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

let mapleader ="\ "			" Leader como espacio

"""""""""""""""""""
" Plugins
"""""""""""""""""""
call plug#begin()
Plug 'junegunn/goyo.vim'
Plug 'kyazdani42/nvim-web-devicons' " optional, for file icons
Plug 'kyazdani42/nvim-tree.lua'
Plug 'chrisbra/Colorizer'
Plug 'Junegunn/fzf.vim'
Plug 'SirVer/ultisnips'
Plug 'dhruvasagar/vim-table-mode'
Plug 'ntk148v/vim-horizon'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'rakr/vim-one'
Plug 'habamax/vim-asciidoctor'
Plug 'morhetz/gruvbox'
Plug 'chriskempson/base16-vim'
Plug 'NLKNguyen/papercolor-theme'
Plug 'sonph/onehalf', { 'rtp': 'vim' }
Plug 'https://gitlab.com/protesilaos/tempus-themes-vim.git'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.0' }
Plug 'nvim-telescope/telescope-file-browser.nvim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope-media-files.nvim'
Plug 'godlygeek/tabular'
Plug 'preservim/vim-markdown'
Plug 'vimpostor/vim-lumen'
call plug#end()

 set nocompatible
    if has("autocmd")
      filetype plugin indent on
    endif

""""""""""""""
"Neovide stuff
""""""""""""""
if exists("g:neovide")
    let g:neovide_refresh_rate=60
    let g:neovide_transparency=1
    let g:neovide_hide_mouse_when_typing = v:true
    let g:neovide_scroll_animation_lenght = 0.3
    let g:neovide_cursor_animation_length=0.05
    let g:neovide_cursor_trail_size=0.8
    let g:neovide_cursor_antialiasing = v:true
    let g:neovide_cursor_vfx_mode = "wireframe"
    let g:neovide_padding_top = 30
    let g:neovide_padding_left = 30
    let g:neovide_padding_right = 30
    let g:neovide_padding_bottom = 30
    set guifont="Fira\ Code\ Medium\ Nerd\ Font\ Complete:h10"
    let g:neovide_scale_factor=0.80
endif

"""""""""""""""""""
" UI
"""""""""""""""""""
set number relativenumber     
set ruler		       " show ruler
set showcmd		       
set incsearch		       " show search live
set hlsearch		       " highlight search
set wildmenu		       " cool menu
set linebreak		       
set hidden
set laststatus=2	       " remove modeline
set textwidth=0		       
set formatoptions+=t
set notermguicolors
syntax enable			
set conceallevel=0

""" Themes """""
let g:grubvox_italic = 1

colorscheme 
highlight Conceal guifg=White guibg=color0
let g:one_allow_italics = 1 
highlight Comment cterm=italic gui=italic
"
""""" Statusline y ruler """""""
"set rulerformat=%14(%l,%c%)\ %P
set statusline=%#Question#%=%#Question#%F%m%r\ %y\ %#VisualNC#\ %l,%c\ \ %P 

"""""""""""""""""
" Funcionamiento
"""""""""""""""""
set autoread			" Actualizar buffers cuando son editados fuera de vim
au FocusGained,BufEnter * checktime
set ignorecase		        " Ignorar mayúsculas cuando busca
set smartcase		        " Si comienzo a buscar en mayúsculas solo da resultados en mayúsculas
set autoindent		        " Sangría automática cuando se crea una nueva debajo de otra ya indentada
set clipboard=unnamedplus       " Portapapeles del sistema activado
set encoding=utf-8	        " Codificación utf8
set shiftwidth=4	        " TAB mide 4 espacios (no estoy seguro)
set softtabstop=4	        " TAB mide 4 espacios (no estoy seguro)
set smartindent		        " Sangría automática en lineas nuevas cerradas por llaves
set smarttab		        " No se, pero sirve para borrar tabs
set virtualedit=block	        " Cursor libre cuando se usa visualblock
set backspace=eol,start,indent  " Backspace funciona bien con tabs
set whichwrap+=<,>,h,l	        " h y j respeta tabs
set undofile			" Conserva el undo después de cerrado el archivo gracias a una caché
set undodir=~/.config/nvim/undodir " La caché a usar
"autocmd FileType markdown TableModeEnable
set mouse=a			" Soporte para mouse
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

au User LumenLight colorscheme onehalflight
au User LumenDark colorscheme gruvbox
"""""""""""""
" Maping
"""""""""""""

" Moverse con jk en lugar de gj y gk 
nnoremap <expr> j (v:count > 4 ? "m'" . v:count . 'j' : 'gj')
nnoremap <expr> k (v:count > 4 ? "m'" . v:count . 'k' : 'gk')
vnoremap <expr> j (v:count > 4 ? "m'" . v:count . 'j' : 'gj')
vnoremap <expr> k (v:count > 4 ? "m'" . v:count . 'k' : 'gk')

"nnoremap <leader><tab> /<++><CR>cw

"Activar Goyo con F6
noremap <F6> :Goyo <CR>
"autocmd! User GoyoEnter Limelight
"autocmd! User GoyoLeave Limelight!
"autocmd  User GoyoLeave 

"Activar spellcheck con F5
noremap <F5> :setlocal spell! spelllang=es<CR>


" Teclas desactivadas
"""""""""""""""""""""
noremap q: <Nop>
nnoremap Q <Nop>


"autocmd FileType markdown inoremap ses<tab> <esc>:read ~/.config/nvim/snips/sesion<CR>kddA 
"autocmd FileType markdown inoremap sec<tab> <esc>:read ~/.config/nvim/snips/secuencia<CR>kdd2wcw 
"autocmd FileType markdown inoremap sec<tab> <esc>:read ~/.config/nvim/snips/secuencia<CR>kdd2wcw 
"
"
let g:asciidoctor_syntax_conceal = 1
let g:asciidoctor_folding = 1
command A2p Asciidoctor2PDF
autocmd FileType asciidoctor map <buffer> <leader>cp :Asciidoctor2PDF<CR>


let g:nnn#command = 'nnn -dcEnHA'
let g:nnn#layout = { 'window': { 'width': 0.9, 'height': 0.6, 'highlight': 'Debug' } }

" Trigger configuration. You need to change this to something other than <tab> if you use one of the following:
" - https://github.com/Valloric/YouCompleteMe
" - https://github.com/nvim-lua/completion-nvim
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"


"markdown stuff

let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_strikethrough = 1


lua <<EOF
require('telescope').setup{
  defaults = {
      hidden = true,
    mappings = {
      i = {
      }
    }
  },
  pickers = {
    -- Default configuration for builtin pickers goes here:
    find_files = {
	theme = "ivy",
	hidden = true,
	},
  },
  extensions = {
      file_browser = {
	  --theme = "ivy",
	  hijack_netrw = true,
	  },
  }
}
require("telescope").load_extension "file_browser"
EOF

noremap <leader><leader> :silent :Telescope find_files<CR>
noremap <leader>f :silent :Telescope file_browser<CR>


