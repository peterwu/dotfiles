#!/bin/bash

yay -S --removemake plymouth

# /etc/default/grub
sudo sed -i.bak \
    -e '/^GRUB_CMDLINE_LINUX_DEFAULT/s/quiet/splash quiet/' \
    /etc/default/grub

sudo grub-mkconfig -o /boot/grub/grub.cfg

# /etc/mkinitcpio.conf
sudo sed -i.bak \
    -e '/^HOOKS=/c HOOKS="base consolefont udev plymouth autodetect modconf keyboard block plymouth-encrypt lvm2 filesystems fsck"' \
    /etc/mkinitcpio.conf

sudo mkinitcpio -p linux
