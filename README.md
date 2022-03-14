## Getting Started

This repo uses stow to manage dotfiles.

1. Use your package manager to install `stow`

```sh
git clone https://github.com/daniebker/dotfiles.git ~/.dotfiles`
cd .dotfiles
git submodule update --init --recursive
stow vim
```

This will add a `.vim` folder one directory up from where you checked out the repository.

If you get an error like

```
~/.dotfiles $ stow vim
WARNING! stowing vim would cause conflicts:
  * existing target is neither a link nor a directory: .vim
```

Its because you have an exisitng folder at ~/.vim. Make a back up of it and try again `mv ~/.vim ~/.vim.v0`

## Required apps

- i3-gaps
- picom

## Installing i3-gaps 

``` sh
sudo apt install libxcb1-dev libxcb-keysyms1-dev libpango1.0-dev libxcb-util0-dev libxcb-icccm4-dev libyajl-dev libstartup-notification0-dev libxcb-randr0-dev libev-dev libxcb-cursor-dev libxcb-xinerama0-dev libxcb-xkb-dev libxkbcommon-dev libxkbcommon-x11-dev xutils-dev libxcb-shape0-dev autoconf
```

``` sh
git clone https://github.com/Airblader/i3.git i3-gaps
cd i3-gaps
mkdir build && cd build
meson --prefix /usr/local
ninja
sudo ninja install
```

### Installing picom

See the [README](https://github.com/yshui/picom) in the repo. 
Ensure all the following dependencies are installed:

``` sh
libxext-dev libxcb1-dev libxcb-damage0-dev libxcb-xfixes0-dev libxcb-shape0-dev libxcb-render-util0-dev libxcb-render0-dev libxcb-randr0-dev libxcb-composite0-dev libxcb-image0-dev libxcb-present-dev libxcb-xinerama0-dev libxcb-glx0-dev libpixman-1-dev libdbus-1-dev libconfig-dev libgl1-mesa-dev libpcre2-dev libpcre3-dev libevdev-dev uthash-dev libev-dev libx11-xcb-dev meson
```

``` sh
git clone https://github.com/yshui/picom.git 
cd picom
git submodule update --init --recursive
meson --buildtype=release . build
ninja -C build
sudo ninja -C install
```

## Google Calendar set up

1. Follow the instructions on the org-gal github repo to set up API credentials
1. Add the following envs vars:

``` sh
export ORG_GCAL_CLIENT_ID
export ORG_GCAL_CLIENT_SECRET
export ORG_GCAL_CALENDAR_EMAIL
```
