#!/bin/bash

sudo pacman -S xorg-server i3-gaps i3blocks i3lock rofi 
sudo pacman -S rxvt-unicode firefox ttf-roboto otf-font-awesome
sudo pacman -S lightdm lightdm-gtk-greeter
sudo pacman -S light playerctl feh compton

sudo systemctl enable lightdm

git clone https://github.com/peterwu/dotfiles.git ~/Projects/dotfiles
