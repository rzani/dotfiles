#!/bin/bash

sudo dnf copr enable gregw/i3desktop
sudo dnf copr enable mhartgring/polybar

sudo dnf install i3-gaps i3blocks polybar feh lxappearance i3lock scrot ranger w3m-img rofi thunar

sudo mkdir ~/.config/polybar
cp /usr/share/doc/polybar/config ~/.config/polybar

# spotlight option:
# Intall albert
sudo dnf config-manager --add-repo https://download.opensuse.org/repositories/home:manuelschneid3r/Fedora_Rawhide/home:manuelschneid3r.repo
sudo dnf install albert

# install compton
sudo dnf install compton

# install nmtui
sudo dnf install NetworkManager-tui   

sudo dnf install playerctl
sudo dnf install pavucontrol

echo "set preview_images true" >> ~/.config/ranger/rc.conf

