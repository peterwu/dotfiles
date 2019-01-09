#!/bin/bash

DISK=/dev/sda
HOST_NAME=arch
ROOT_PASSWORD=root_password
USER_NAME=peter
USER_PASSWORD=user_password

sed -i.bak \
    -e '/^#en_CA/s/^#//' \
    /etc/locale.gen
locale-gen
locale > /etc/locale.conf
ln -sf /usr/share/zoneinfo/America/Toronto /etc/localtime
hwclock -wu

hostnamectl set-hostname $HOST_NAME
echo "127.0.1.1 $HOST_NAME.local $HOST_NAME" >> /etc/hosts

sed -i.bak \
    -e '/^HOOKS=/c HOOKS="base consolefont udev autodetect modconf keyboard block encrypt lvm2 filesystems fsck"' \
    -e '/^#COMPRESSION="lz4"/s/^#//' \
    /etc/mkinitcpio.conf 

# ready the kernel
mkinitcpio -p linux
pacman -S --noconfirm intel-ucode

# install terminus font
pacman -S --noconfirm terminus-font

# change root password
echo $ROOT_PASSWORD | passwd

pacman -S --noconfirm grub efibootmgr
grub-install --target=i386-pc $DISK
# grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=grub 
grub-mkfont -s 32 -o /boot/grub/fonts/font.pf2 /usr/share/fonts/misc/ter-x32n.pcf.gz

# grub
UUID=$(blkid $DISK"2" | awk '{print $2}' | sed -e 's/"//g')
sed -i.bak \
    -e '/^GRUB_CMDLINE_LINUX_DEFAULT=/c GRUB_CMDLINE_LINUX_DEFAULT=cryptdevice=$UUID:vg splash quiet' \
    /etc/default/grub
echo "GRUB_FONT=/boot/grub/fonts/font.pf2" >> /etc/default/grub
echo "KEYMAP=us" > /etc/vconsole.conf
echo "FONT=ter-x32n" >> /etc/vconsole.conf
grub-mkconfig -o /boot/grub/grub.cfg

# add user
useradd -m -g users -G wheel -s /bin/bash $USER_NAME
echo $USER_PASSWORD | passwd $USER_NAME

# install more software
pacman -S --noconfirm xorg-server networkmanager
pacman -S --noconfirm i3-gaps i3blocks i3lock rofi
pacman -S --noconfirm lightdm lightdm-gtk-greeter
pacman -S --noconfirm tlp

# enable systemd daemons
systemctl enable lightdm.service
systemctl enable NetworkManager.service
systemctl enable fstrim.timer
systemctl enable tlp tlp-sleep

# exit chroot
exit

