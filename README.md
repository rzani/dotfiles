# Project Dotfiles Configuration

## i3wm

i3wm is a tiling window manager, designed for X11, inspired by wmii and written in C. It supports tiling, stacking, and tabbing layouts, which it handles dynamically.

### Installation

The following steps describe how to install i3wm and its dependencies based on the `i3/install.sh` script. This setup is primarily for Fedora.

1.  **Enable COPR Repositories**:
    ```bash
    sudo dnf copr enable gregw/i3desktop
    sudo dnf copr enable mhartgring/polybar
    ```

2.  **Install Packages**:
    Install i3-gaps, i3blocks, Polybar, feh (for wallpaper), lxappearance (for GTK themes), i3lock (for screen locking), scrot (for screenshots), ranger (file manager), w3m-img (for image previews in ranger), rofi (launcher), and thunar (file manager).
    ```bash
    sudo dnf install i3-gaps i3blocks polybar feh lxappearance i3lock scrot ranger w3m-img rofi thunar
    ```

3.  **Configure Polybar**:
    Create the Polybar configuration directory and copy the default configuration file.
    ```bash
    sudo mkdir ~/.config/polybar
    cp /usr/share/doc/polybar/config ~/.config/polybar
    ```

4.  **Install Albert (Spotlight Alternative)**:
    Add the Albert repository and install it.
    ```bash
    sudo dnf config-manager --add-repo https://download.opensuse.org/repositories/home:manuelschneid3r/Fedora_Rawhide/home:manuelschneid3r.repo
    sudo dnf install albert
    ```

5.  **Install Compton (Compositor)**:
    ```bash
    sudo dnf install compton
    ```

6.  **Install NetworkManager-tui**:
    For managing network connections via a terminal UI.
    ```bash
    sudo dnf install NetworkManager-tui
    ```

7.  **Install Playerctl**:
    For controlling media players.
    ```bash
    sudo dnf install playerctl
    ```

8.  **Install Pavucontrol**:
    PulseAudio volume control.
    ```bash
    sudo dnf install pavucontrol
    ```

9.  **Configure Ranger**:
    Enable image previews in ranger.
    ```bash
    echo "set preview_images true" >> ~/.config/ranger/rc.conf
    ```

### Keybindings

The following are important custom keybindings defined in the `i3/config` file. The Mod key (`$mod`) is typically Mod4 (Super/Windows key).

*   **Launch Terminal**: `$mod+Return`
*   **Kill Window**: `$mod+Shift+q`
*   **Program Launcher (Albert)**: `$mod+d`
*   **Focus Movement**:
    *   `$mod+h` or `$mod+Left`: Focus left
    *   `$mod+j` or `$mod+Down`: Focus down
    *   `$mod+k` or `$mod+Up`: Focus up
    *   `$mod+l` or `$mod+Right`: Focus right
*   **Moving Windows**:
    *   `$mod+Shift+h` or `$mod+Shift+Left`: Move left
    *   `$mod+Shift+j` or `$mod+Shift+Down`: Move down
    *   `$mod+Shift+k` or `$mod+Shift+Up`: Move up
    *   `$mod+Shift+l` or `$mod+Shift+Right`: Move right
*   **Splitting Windows**:
    *   `$mod+g`: Split horizontal
    *   `$mod+v`: Split vertical
*   **Fullscreen**: `$mod+f` (toggle)
*   **Layout Modes**:
    *   `$mod+s`: Stacking
    *   `$mod+w`: Tabbed
    *   `$mod+e`: Toggle split
*   **Floating Mode**: `$mod+Shift+space` (toggle)
*   **Workspace Switching**: `$mod+<number>` (e.g., `$mod+1` for workspace 1)
*   **Moving Windows to Workspaces**: `$mod+Shift+<number>` (e.g., `$mod+Shift+1` to move window to workspace 1)
*   **Reload i3 Config**: `$mod+Shift+c`
*   **Restart i3**: `$mod+Shift+r`
*   **Exit i3**: `$mod+Shift+e`
*   **Resize Mode**: `$mod+r` (then use `h/j/k/l` or arrow keys to resize)
*   **Screen Lock**: `$mod+Shift+x` (executes `i3/lock.sh`)
*   **Screenshot (Selection)**: `$mod+Shift+s` (uses `scrot -s -d 2`)
*   **Audio Controls**:
    *   `XF86AudioRaiseVolume`: Increase volume
    *   `XF86AudioLowerVolume`: Decrease volume
    *   `XF86AudioMute`: Toggle mute
*   **Brightness Controls**:
    *   `XF86MonBrightnessUp`: Increase brightness
    *   `XF86MonBrightnessDown`: Decrease brightness
*   **Media Player Controls**:
    *   `XF86AudioPlay`: Play
    *   `XF86AudioPause`: Pause
    *   `XF86AudioNext`: Next track
    *   `XF86AudioPrev`: Previous track

### Screen Locking (`i3/lock.sh`)

The script `i3/lock.sh` is used to lock the screen. It performs the following actions:

1.  Takes a screenshot of the current screen using `scrot`.
2.  Blurs the screenshot using ImageMagick's `convert` tool (Note: The `install.sh` doesn't explicitly install ImageMagick, which might be a missing dependency for the lock script to fully work as intended with blur).
3.  Overlays an icon (e.g., a lock icon, assuming `icon.png` exists in the i3 config directory) onto the blurred screenshot.
4.  Uses `i3lock` with the modified screenshot as the lock screen background.
5.  Pauses Spotify playback using `playerctl -p spotify pause` if Spotify is running.

*The `lock.sh` script content is not provided, so the above explanation is based on common practices for such scripts. The actual script might differ.*

### Startup Applications

The i3 configuration (`i3/config`) launches several applications and services when i3 starts:

*   **Polybar**: Launched using `exec_always --no-startup-id $HOME/.config/polybar/launch.sh`. This script is responsible for starting the Polybar status bar.
*   **Compton**: `exec compton` starts the compositor for visual effects like transparency.
*   **Albert**: `exec albert` starts the keyboard launcher.
*   **Wallpaper**: `exec_always feh --bg-scale $wallpaper` sets the desktop wallpaper using `feh`. The `$wallpaper` variable is set to `$HOME/wallpaper.jpg` in the config.

This provides a comprehensive overview of the i3wm setup defined in the provided configuration files.

## Polybar

Polybar is a fast and easy-to-use tool for creating status bars.

### Launching Polybar

Polybar is launched via the `polybar/launch.sh` script, which is executed when i3wm starts.
This script first terminates any existing Polybar instances and then launches the main bar defined in the configuration:
```bash
polybar -r -c ~/.config/polybar/config.ini main &
```
The `-r` flag tells Polybar to reload the configuration, and `-c` specifies the path to the main configuration file. `main` refers to the bar named `[bar/main]` within the configuration.

### Configuration Structure

The Polybar configuration is modular and split into several files, all located under `~/.config/polybar/`:

*   **`polybar/config.ini`**: This is the main configuration file.
    *   It includes other configuration files:
        ```ini
        include-file = ~/.config/polybar/colors.ini
        include-file = ~/.config/polybar/modules.ini
        include-file = ~/.config/polybar/user_modules.ini
        include-file = ~/.config/polybar/bars.ini
        ```
    *   Defines the main bar settings under `[bar/main]`, such as:
        *   `width`, `height`, `offset-x`, `offset-y`
        *   `background`, `foreground` (often referencing colors from `colors.ini`)
        *   `font-0`, `font-1`, etc., for specifying fonts.
        *   `modules-left`, `modules-center`, `modules-right` which list the modules to display in each section of the bar.
    *   Specifies global WM settings and application settings for Polybar itself.

*   **`polybar/colors.ini`**:
    *   Contains all color definitions used throughout the Polybar configuration (e.g., `bg`, `fg`, `ac`, various material design color shades).
    *   Example:
        ```ini
        [color]
        bg = #222629
        fg = #c1c1c1
        ac = #364855
        ```

*   **`polybar/modules.ini`**:
    *   Defines the configuration for standard Polybar internal modules. Examples found in this configuration include:
        *   `[module/alsa]`: Sound volume (ALSA).
        *   `[module/backlight]`: Screen brightness control.
        *   `[module/battery]`: Battery status.
        *   `[module/cpu]`: CPU usage.
        *   `[module/date]`: Current time and date.
        *   `[module/filesystem]`: Filesystem usage.
        *   `[module/memory]`: Memory usage.
        *   `[module/network]` (can be configured for wired or wireless): Network status, ESSID, and connection speed.
        *   `[module/temperature]`: System temperature.
        *   `[module/keyboard]`: Keyboard layout and indicators (e.g., Caps Lock).
        *   `[module/title]`: Active window title.
        *   `[module/workspaces]`: i3 workspace indicators.

*   **`polybar/user_modules.ini`**:
    *   Contains definitions for custom modules, typically based on scripts. Examples in this configuration:
        *   `[module/checknetwork]`:
            *   Displays network status.
            *   Script: `polybar/scripts/check-network`.
            *   Clickable to open `networkmanager_dmenu`.
        *   `[module/updates]`:
            *   Shows the number of available system updates.
            *   Script: `polybar/scripts/updates.sh`.
            *   Clickable to run `polybar/scripts/lupdates` (likely to view or apply updates).
        *   `[module/launcher]`:
            *   An icon that acts as an application launcher.
            *   Scripts: `polybar/scripts/launcher`, `polybar/scripts/launcher-alt`, `polybar/scripts/launcher-full`.
        *   `[module/sysmenu]`:
            *   An icon for a system menu (e.g., power off, reboot).
            *   Scripts: `polybar/scripts/powermenu`, `polybar/scripts/powermenu-alt`.
        *   `[module/colors-switch]`:
            *   An icon to trigger a color scheme switch.
            *   Script: `polybar/scripts/color-switch.sh`.
        *   `[module/info-trash]`:
            *   Displays information about the trash directory (e.g., size).
            *   Script: `polybar/scripts/info-trash.sh`.
            *   Clickable to clean the trash.

*   **`polybar/bars.ini`**:
    *   This file can define specific bar sections or provide overrides for module appearances within a bar. In this setup, it seems to contain alternative module definitions with bar-like visualizations (e.g., `[module/volume]` with `bar-volume-...` settings, `[module/cpu_bar]`, `[module/memory_bar]`, `[module/filesystem_bar]`, `[module/mpd_bar]`). These provide graphical representations (like progress bars) for various system stats.

### Fonts

*   Fonts are specified in `polybar/config.ini` under the `[bar/main]` section (e.g., `font-0 = "Ubuntu Condensed:size=10;2"`).
*   Custom fonts (like icon fonts) used in the configuration are stored in the `polybar/fonts/` directory (e.g., `icomoon-feather.ttf`).

### Scripts

The `polybar/scripts/` directory contains various helper scripts used by the custom modules:

*   `check-network`: Checks and displays network connectivity.
*   `updates.sh`: Checks for system updates (likely using a package manager).
*   `lupdates`: Likely displays or initiates the update process.
*   `launcher`, `launcher-alt`, `launcher-full`: Scripts for different application launcher behaviors (e.g., Rofi).
*   `powermenu`, `powermenu-alt`: Scripts for system power options (shutdown, reboot, etc.).
*   `color-switch.sh`: Script to change the Polybar (and potentially other applications') color scheme.
    *   This script likely calls other scripts like `colors-dark.sh` and `colors-light.sh` to apply different themes.
*   `info-trash.sh`: Manages and displays trash directory status.
*   `checkupdates`: (Likely related to `updates.sh`)
*   `windows`: (Purpose unclear from filename alone, might be related to window management or switching).

## Zsh and Oh My Zsh

Zsh is a powerful shell that operates as both an interactive shell and a scripting language interpreter. Oh My Zsh is an open-source, community-driven framework for managing Zsh configuration.

### Installation

1.  **Install Zsh**:
    *   On Fedora:
        ```bash
        sudo dnf install zsh
        ```
    *   On Debian/Ubuntu:
        ```bash
        sudo apt install zsh
        ```
    After installation, you might want to change your default shell to Zsh:
    ```bash
    chsh -s $(which zsh)
    ```
    You'll need to log out and log back in for this change to take effect.

2.  **Install Oh My Zsh**:
    Oh My Zsh is typically installed using a command from their website. The common method is:
    ```bash
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    ```
    Alternatively, using `wget`:
    ```bash
    sh -c "$(wget https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh -O -)"
    ```
    Refer to the [Oh My Zsh website](https://ohmyz.sh/) for the most current installation instructions.

### Configuration (`~/.zshrc`)

The main Zsh configuration file is `~/.zshrc`. This file is sourced when Zsh starts. For users of Oh My Zsh, this file is where you customize themes, plugins, and other settings.

This dotfiles setup uses a `zshrc` file that configures Oh My Zsh.

#### Oh My Zsh Theme

The theme is set using the `ZSH_THEME` variable:
```zsh
ZSH_THEME="amuse"
```

#### Oh My Zsh Plugins

Plugins enhance the functionality of the shell. They are enabled by adding them to the `plugins` array in `~/.zshrc`:
```zsh
plugins=(
    git
    history-substring-search
    colored-man-pages
    web-search
    docker-machine
)
```
*   `git`: Adds many aliases and useful functions for Git.
*   `history-substring-search`: Allows you to search your command history by typing a substring and then pressing Up/Down arrows.
*   `colored-man-pages`: Adds colors to `man` pages for better readability.
*   `web-search`: Adds commands to search the web (e.g., `google search_term`).
*   `docker-machine`: Adds autocompletion for Docker Machine.

### Custom Aliases (`alias.sh`)

Custom command aliases are defined in the `alias.sh` file, which is sourced at the end of the `~/.zshrc` file:
```zsh
source ~/dotfiles/alias.sh
```
*(Note: The path `~/dotfiles/alias.sh` assumes these dotfiles are cloned into a directory named `dotfiles` in the user's home directory. Adjust if your setup differs.)*

Here are some of the aliases defined in `alias.sh`:

*   **APT (Package Management)**:
    *   `api`: `sudo apt install` - Install packages.
    *   `apu`: `sudo apt update` - Update package lists.
    *   `apug`: `sudo apt upgrade` - Upgrade installed packages.
*   **Git**:
    *   `gpu`: `git push --set-upstream origin <current_branch>` - Push current branch to origin and set upstream.
*   **Docker**:
    *   `dps`: `docker ps --format 'table{{.ID}}	{{.Names}}	{{.Ports}}'` - List running containers with custom format.
    *   `dpsa`: `docker ps -a` - List all containers (running and stopped).
    *   `dimg`: `docker images` - List Docker images.
    *   `dexec`: `docker exec -it <container_id_or_name>` - Execute a command in a running container.
    *   `dn`: `docker network` - Manage Docker networks.
    *   `dc`: `docker container` - Manage Docker containers.
    *   `dl`: `docker logs <container_id_or_name>` - Fetch logs of a container.
*   **Go**:
    *   `go-coverage`: `go test -coverprofile=coverage.out && go tool cover -html=coverage.out && rm -rf coverage.out` - Run Go tests, generate a coverage report, open it in a browser, and clean up.
*   **Python**:
    *   `p3`: `python3` - Shortcut for `python3`.
*   **Laradock (Development Environment)**:
    *   `dev-up`: `cd ~/Workspace/webserver/dev && docker-compose up -d nginx mariadb redis && cd -` - Start Laradock services (nginx, MariaDB, Redis).
    *   `dev-start`: Same as `dev-up`.
    *   `dev-stop`: `cd ~/Workspace/webserver/dev && docker-compose stop && cd -` - Stop Laradock services.
    *   `dev-restart`: `cd ~/Workspace/webserver/dev && docker-compose restart && cd -` - Restart Laradock services.
    *   `dev-bash`: `cd ~/Workspace/webserver/dev && docker-compose exec --user=laradock workspace bash && cd -` - Bash into the Laradock workspace container.
*   **Files & Configuration**:
    *   `hosts`: `sudo vim /etc/hosts` - Edit the `/etc/hosts` file.
    *   `zshconfig`: `vim ~/.zshrc` - Edit the Zsh configuration file.
*   **Tmux (Terminal Multiplexer)**:
    *   `t`: `tmux` - Start a new Tmux session or attach to the last one.
    *   `ta`: `tmux a` - Attach to the last Tmux session.
    *   `tls`: `tmux ls` - List Tmux sessions.
    *   `tn`: `tmux new -s <session_name>` - Create a new named Tmux session.
*   **Servers (SSH Shortcuts)**:
    *   `ns2`: `ssh ns2 -t byobu` - SSH to server `ns2` and start/attach to a `byobu` session.
    *   `ns1`: `ssh ns1 -t byobu` - SSH to server `ns1` and start/attach to a `byobu` session.

## Vim Configuration (`vimrc`)

This setup uses Vim with a configuration managed in the `vimrc` file.

### Installation

Ensure Vim or Neovim is installed. For example, on Debian/Ubuntu:
```bash
sudo apt install vim
# or for Neovim
sudo apt install neovim
```

### Plugin Management: Vim-Plug

[Vim-Plug](https://github.com/junegunn/vim-plug) is used for managing Vim plugins.

*   **Installation**: Vim-Plug is installed by placing `plug.vim` in `~/.vim/autoload/plug.vim` (for Vim) or `~/.local/share/nvim/site/autoload/plug.vim` (for Neovim). The `vimrc` includes a command to automatically install it if missing:
    ```bash
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    # For Neovim (the vimrc checks both paths):
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    ```
    Plugins are installed automatically when Vim starts if Vim-Plug was just installed, or by running `:PlugInstall`.

*   **Configured Plugins**:
    ```vim
    Plug 'mhinz/vim-startify'
    Plug 'easymotion/vim-easymotion'
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'
    Plug 'scrooloose/nerdcommenter'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'sheerun/vim-polyglot'        " Syntax highlighting for many languages
    Plug 'vim-scripts/grep.vim'
    Plug 'jiangmiao/auto-pairs'        " Automatically inserts closing pairs for quotes, brackets, etc.
    Plug 'editorconfig/editorconfig-vim' " Maintains consistent coding styles
    Plug 'arnaud-lb/vim-php-namespace' " Helps with PHP namespaces
    Plug 'Xuyuanp/nerdtree-git-plugin' " Git status indicators for NERDTree
    Plug 'mattn/emmet-vim'             " Expands abbreviations into HTML/CSS
    Plug 'Shougo/neosnippet.vim'       " Snippet engine
    Plug 'Shougo/neosnippet-snippets'  " Default snippets for neosnippet
    Plug 'majutsushi/tagbar'           " Displays tags in a sidebar (classes, functions, etc.)
    Plug 'terryma/vim-multiple-cursors' " Sublime Text-like multiple cursors
    Plug 'posva/vim-vue'               " Vue.js syntax highlighting and indentation
    Plug 'tpope/vim-surround'          " Mappings to easily delete, change and add 'surroundings'
    Plug 'tpope/vim-repeat'            " Enables repeating plugin commands with '.'
    Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' } " Go development plugin
    Plug 'zchee/deoplete-go', { 'do': 'make'}          " Go completion for deoplete
    Plug 'francoiscabrol/ranger.vim'   " Integrates Ranger file explorer
    Plug 'rbgrouleff/bclose.vim'       " Better buffer closing management

    " Neovim specific for deoplete, with fallback for Vim
    if has('nvim')
        Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    else
        Plug 'Shougo/deoplete.nvim' " Completion framework
        Plug 'roxma/nvim-yarp'      " Yet another RPC plugin (for Neovim compatibility)
        Plug 'roxma/vim-hug-neovim-rpc' " RPC support for Vim
    endif
    ```

### Core Vim Settings

*   **Leader Key**: The leader key is set to comma:
    ```vim
    let mapleader=','
    ```
*   **Mouse Support**: Enabled in all modes:
    ```vim
    set mouse=a
    ```
*   **Encoding**: UTF-8 is enforced:
    ```vim
    set encoding=utf-8
    set fileencoding=utf-8
    set fileencodings=utf-8
    set bomb " Bill of Materials
    set binary
    ```
*   **Backup and Swap Files**: Backups and swap files are disabled. Undo history is enabled and stored in `/tmp/`:
    ```vim
    set nobackup
    set noswapfile
    set undofile
    set undodir=/tmp
    ```
*   **Search Behavior**:
    *   `set hlsearch`: Highlight search matches.
    *   `set incsearch`: Show matches incrementally while typing.
    *   `set ignorecase`: Ignore case in search patterns.
    *   `set smartcase`: Override `ignorecase` if the pattern contains uppercase letters.
*   **Tabs and Indentation**:
    *   `set tabstop=4`: Tabs are 4 spaces wide.
    *   `set shiftwidth=4`: Indentation is 4 spaces wide.
    *   `set expandtab`: Use spaces instead of tab characters.
    *   `set ai`: Auto indent.
    *   `set si`: Smart indent.
*   **Buffer Behavior**:
    *   `set hidden`: Allows switching buffers without saving.
    *   `set splitbelow`: When splitting, the new window appears below the current one.
*   **Other**:
    *   `set autowrite`: Automatically write buffer if changed, before some commands.
    *   `set ttyfast`: Faster redrawing (especially over slow connections).
    *   `set lazyredraw`: Reduces redrawing during macros.
    *   `set fileformats=unix,dos,mac`: File format detection.
    *   `set showcmd`: Show command info in the status line.
    *   `set shell=/bin/zsh`: Sets default shell.

### Visual Settings

*   `syntax on`: Enable syntax highlighting.
*   `set ruler`: Show cursor position in the status line.
*   `set number`: Show line numbers.
*   `set relativenumber`: Show relative line numbers.
*   `set wildmenu`: Enhanced command-line completion.
*   `set wildmode=longest:full,full`: Command-line completion mode.
*   `set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite,*.tar.*`: Ignore certain files during wildcard expansion.
*   `set background=light`: Assumes a light background (can be changed by color schemes).
*   `set scrolloff=3`: Keep 3 lines visible above/below cursor when scrolling.
*   `set laststatus=2`: Always show the status bar.
*   `set gcr=a:blinkon0`: Disable cursor blinking.
*   `set title`: Set terminal title to filename.

### Key Plugin Configurations

*   **vim-airline**:
    *   Theme: `let g:airline_theme = 'angr'`
    *   Extensions: `ale` and `branch` enabled. `tabline` and `tagbar` extensions are disabled.
*   **ctrlp.vim**:
    *   Ignored patterns: `node_modules`, `DS_Store`, `git`.
    *   Uses `ag` (The Silver Searcher) for file searching if `ag` is executable, which is faster and respects `.gitignore`.
*   **deoplete.nvim** (Completion Framework for Neovim/Vim):
    *   Enabled at startup: `let g:deoplete#enable_at_startup = 1`
    *   Smart case enabled: `let g:deoplete#enable_smart_case = 1`
    *   Go specific settings:
        *   `let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'`
        *   `let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']`
*   **vim-startify**:
    *   Custom start screen with sections for sessions, MRU files, MRU directory, bookmarks, and commands.
*   **ranger.vim**:
    *   `let g:ranger_map_keys = 0`: Disables default Ranger key mappings to avoid conflicts.
*   **neosnippet.vim**:
    *   Snippets directory: `let g:neosnippet#snippets_directory='~/.vim/snippets'`
    *   snipMate compatibility enabled: `let g:neosnippet#enable_snipmate_compatibility = 1`
    *   User-defined snippets should be placed in `~/.vim/snippets`.
*   **grep.vim**:
    *   Default options: `let Grep_Default_Options = '-IR'` (recursive, ignore binary files).
    *   Skips files: `*.log`, `*.db`.
    *   Skips directories: `.git`, `node_modules`, `vendor`, `storage`.

### Important Mappings

*   **General**:
    *   `jj`, `jk`, `kk` to `<Esc>` in Insert mode for easier mode switching.
    *   `<Leader>w`: `:w!` (Save file).
    *   `-`: `:Ranger<CR>` (Open Ranger file explorer).
    *   `<C-P>`: `:CtrlP<CR>` (Open CtrlP file finder).
    *   `<Leader>r`: `:CtrlPMRUFiles<CR>` (CtrlP Most Recently Used files).
    *   `<Tab>`: `:CtrlPBuffer<CR>` (CtrlP open buffers). (Note: `vimrc` has `<Leader><Tab>` for this, `<Tab>` might be for completion)
    *   `<Leader>q`: `:bp | bd! #<CR>` (Close current buffer).
    *   `<Leader>ba`: `:bufdo bd<CR>` (Close all buffers).
    *   `<Leader><Space>`: `:noh<CR>` (Clear search highlight).
    *   `<Leader>,`: `:lcd %:p:h<CR>` (Set working directory to current file's directory).
    *   `<C-Up>` / `<C-Down>`: Move current line up/down in Normal and Insert mode.
    *   `<Leader>i`: `mzgg=G\`z` (Indent whole buffer).
    *   `<F8>`: `:TagbarOpenAutoClose<CR>` (Toggle Tagbar).
    *   `<Leader>e`: `:Ranger<CR>` (Open Ranger, alternative to `-`).
    *   `<Leader>vi`: `:e $MYVIMRC<CR>` (Edit `vimrc`).
*   **Git (vim-fugitive)**:
    *   `<Leader>ga`: `:Gwrite` (Stage current file).
    *   `<Leader>gc`: `:Gcommit` (Commit staged changes).
    *   `<Leader>gp`: `:Gpush` (Push changes).
    *   `<Leader>gl`: `:Gpull` (Pull changes).
    *   `<Leader>gs`: `:Gstatus` (Open Git status window).
    *   `<Leader>gbh`: `:Gblame` (Open Git blame).
    *   `<Leader>gd`: `:Gvdiff` (Open Git diff in vertical split).
*   **Go (vim-go)**:
    *   `<Leader>b`: `<Plug>(go-build)` (Build Go project).
    *   `<Leader>r`: `<C-W>o<Plug>(go-run)` (Run Go project).
    *   `<Leader>t`: `<Plug>(go-test)` (Run Go tests).
    *   `<Leader>ds`: `<Plug>(go-def)` (Go to definition).
    *   `<Leader>dv`: `<Plug>(go-def-vertical)` (Go to definition in vertical split).
    *   `<Leader>dt`: `<Plug>(go-def-tab)` (Go to definition in new tab).
*   **PHP (vim-php-namespace)**:
    *   `<Leader>u`: Insert PHP `use` statement (both normal and insert mode).
*   **Neosnippet**:
    *   `<C-k>`: Expand snippet or jump to next placeholder in Insert, Select, and Visual modes.
*   **Easymotion**:
    *   `<Space>s`: `<Plug>(easymotion-overwin-f)` (Find character across all windows).
    *   `<Space><Space>`: `<Plug>(easymotion-sn)` (Find character forwards).
*   **Neovim Terminal Mappings**:
    *   `<C-[>`: `<C-\><C-n>` (Exit Terminal mode to Normal mode).
    *   `<M-h/j/k/l>`: Navigate between Vim splits from Terminal, Insert, Visual, and Normal modes.

### Autocommands

*   **Auto-source vimrc**: Automatically sources the `vimrc` (or `init.vim` for Neovim) after it's saved.
    ```vim
    augroup autosourcing
        autocmd!
        if has('nvim')
            autocmd BufWritePost init.vim source %
        else
            autocmd BufWritePost .vimrc source %
        endif
    augroup END
    ```
*   **Remember Cursor Position**: Restores cursor to the last known position when opening a file.
    ```vim
    augroup vimrc-remember-cursor-position
        autocmd!
        autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
    augroup END
    ```
This configuration provides a feature-rich Vim/Neovim environment tailored for development.

## Tmux Configuration (`tmux.conf`)

Tmux is a terminal multiplexer that allows you to manage multiple terminal sessions and windows from a single screen.

### Installation

Tmux can be installed using your system's package manager. For example:
*   On Fedora: `sudo dnf install tmux`
*   On Debian/Ubuntu: `sudo apt install tmux`

### Prefix

The default Tmux prefix `Ctrl+b` has been remapped to:
*   **`Ctrl+Space`**

### Core Settings & Behavior

*   **Default Terminal**:
    ```tmux
    set -g default-terminal 'screen-256color'
    ```
*   **Window/Pane Indexing**: Windows and panes start indexing from `1` instead of `0`.
    ```tmux
    set -g base-index 1
    set-window-option -g pane-base-index 1
    ```
*   **Window Renumbering**: Windows are automatically renumbered sequentially when a window is closed.
    ```tmux
    set -g renumber-windows on
    ```
*   **History Limit**:
    ```tmux
    set -g history-limit 10000
    ```
*   **Mouse Mode**: Mouse support is enabled for scrolling, pane selection, and resizing.
    ```tmux
    set -g mouse on
    ```
*   **Default Shell**: Uses the system's default shell (`$SHELL`).
    ```tmux
    set -g default-shell "$SHELL"
    ```
*   **Window Renaming**: Automatic window renaming is disabled.
    ```tmux
    set -g allow-rename off
    ```

### Keybindings

*   **Navigation**:
    *   Vim-like pane selection using the prefix:
        *   `<Prefix> h`: Select pane to the left.
        *   `<Prefix> j`: Select pane below.
        *   `<Prefix> k`: Select pane above.
        *   `<Prefix> l`: Select pane to the right.
    *   Previous/Next window:
        *   `<Prefix> Ctrl+h`: Previous window.
        *   `<Prefix> Ctrl+l`: Next window.
    *   Last active pane:
        *   `<Prefix> Ctrl+Space`: Switch to the last active pane.
*   **Pane Resizing** (no prefix needed):
    *   `Shift + Left`: Resize current pane left.
    *   `Shift + Right`: Resize current pane right.
    *   `Shift + Up`: Resize current pane up.
    *   `Shift + Down`: Resize current pane down.
*   **Smart Pane Switching (Vim-aware)** (no prefix needed):
    These bindings check if the current pane is running Vim. If so, the key presses are sent to Vim; otherwise, they switch Tmux panes.
    *   `Ctrl + h`: Send `Ctrl+h` to Vim or select pane to the left.
    *   `Ctrl + j`: Send `Ctrl+j` to Vim or select pane below.
    *   `Ctrl + k`: Send `Ctrl+k` to Vim or select pane above.
    *   `Ctrl + l`: Send `Ctrl+l` to Vim or select pane to the right.
    *   `Ctrl + \`: Send `Ctrl+\` to Vim or select the last active pane.
*   **Session & Window Management**:
    *   Reload Tmux config: `<Prefix> r` (sources `~/.tmux.conf`).
    *   Split pane vertically: `<Prefix> _` (opens in current path).
    *   Split pane horizontally: `<Prefix> |` (opens in current path).
    *   New window: `<Prefix> c` (opens in current path).
    *   Kill current session: `<Prefix> K`.
*   **Other**:
    *   Edit todo list: `<Prefix> t` (opens `~/todo.org` in Vim in a new horizontal split).

### Plugin Management (TPM - Tmux Plugin Manager)

[TPM (Tmux Plugin Manager)](https://github.com/tmux-plugins/tpm) is used to manage Tmux plugins.

*   **Installation**:
    Clone the TPM repository:
    ```bash
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    ```
*   **Usage**:
    1.  Add plugin lines to your `~/.tmux.conf`: `set -g @plugin 'user/repository'`.
    2.  Press `<Prefix> + I` (Shift+i) to fetch and install new plugins.
    3.  Plugins are sourced automatically when Tmux starts.

*   **Configured Plugins**:
    ```tmux
    set -g @plugin 'tmux-plugins/tpm'                 # Plugin manager
    set -g @plugin 'tmux-plugins/tmux-sensible'       # Sensible Tmux defaults
    set -g @plugin 'tmux-plugins/tmux-open'           # Open highlighted selection
    set -g @plugin 'tmux-plugins/tmux-resurrect'      # Persists Tmux environment across reboots
    set -g @plugin 'tmux-plugins/tmux-continuum'      # Continuous saving and auto-starting
    set -g @plugin 'tmux-plugins/tmux-cpu'            # CPU usage in status bar
    set -g @plugin 'tmux-plugins/tmux-prefix-highlight' # Highlights when prefix is pressed
    set -g @plugin 'tmux-plugins/tmux-yank'           # Copying to system clipboard
    set -g @plugin 'tmux-plugins/tmux-copycat'        # Enhanced search capabilities
    ```
    *   **tmux-continuum Settings**:
        *   Auto-starts Tmux on system boot: `set -g @continuum-boot 'on'`
        *   Restores previous Tmux environment when Tmux is started: `set -g @continuum-restore 'on'`
        *   Auto-saves sessions every 5 minutes: `set -g @continuum-save-interval '5'`
        *   Vim/Neovim session saving strategy: `set -g @resurrect-strategy-vim 'session'`, `set -g @resurrect-strategy-nvim 'session'`

### Appearance and Theming

The `tmux.conf` includes settings for a custom appearance, likely inspired by Solarized, using color variables (e.g., `$base03`, `$yellow`, `$orange`).

*   **Status Bar**:
    *   The `status-right` is customized to show: Prefix highlight, CPU percentage, and Date/Time.
        ```tmux
        set -g status-right '#{prefix_highlight} #[fg=colour15,bg=colour81] CPU#{cpu_percentage} #[fg=colour15,bg=colour241,bold] %d/%m #[fg=colour8,bg=colour245,bold] %H:%M '
        ```
    *   Status bar is updated every 2 seconds (`status-interval 2`).
    *   Colors are set for status background/foreground, window status (default and current), pane borders, and message text.

### TPM Initialization

The following line must be at the very bottom of your `~/.tmux.conf` to initialize TPM:
```tmux
run-shell '~/.tmux/plugins/tpm/tpm'
```
This ensures all plugins are loaded correctly.
