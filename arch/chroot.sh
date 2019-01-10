#!/bin/bash

DISK=/dev/sda
HOST_NAME=arch
ROOT_PASSWORD=root_password
USER_NAME=peter
USER_PASSWORD=user_password

sed -i.bak \
    -e '/^#en_CA.UTF/s/^#//' \
    /etc/locale.gen
locale-gen
# locale > /etc/locale.conf
echo 'LANG="en_CA.UTF-8"' > /etc/locale.conf
ln -sf /usr/share/zoneinfo/America/Toronto /etc/localtime
hwclock -wu

echo $HOST_NAME > /etc/hostname
echo "127.0.1.1 $HOST_NAME.local $HOST_NAME" >> /etc/hosts

sed -i.bak \
    -e '/^HOOKS=/c HOOKS="base consolefont udev autodetect modconf keyboard block encrypt lvm2 filesystems fsck"' \
    -e '/^#COMPRESSION="lz4"/s/^#//' \
    /etc/mkinitcpio.conf 

# install terminus font
pacman -S --noconfirm terminus-font
echo "KEYMAP=us" > /etc/vconsole.conf
echo "FONT=ter-m32n" >> /etc/vconsole.conf

# ready the kernel
mkinitcpio -p linux
pacman -S --noconfirm intel-ucode

# change root password
echo root:$ROOT_PASSWORD | chpasswd

# grub
pacman -S --noconfirm grub efibootmgr
UUID=$(blkid $DISK"2" | awk '{print $2}' | sed -e 's/"//g')
grub-install --force --target=i386-pc $DISK
# grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=grub 
grub-mkfont -s 32 -o /boot/grub/fonts/font.pf2 /usr/share/fonts/misc/ter-x32n.pcf.gz
echo "GRUB_FONT=/boot/grub/fonts/font.pf2" >> /etc/default/grub
sed -i.bak \
    -e "/^GRUB_CMDLINE_LINUX_DEFAULT=/c GRUB_CMDLINE_LINUX_DEFAULT=\"cryptdevice=$UUID:vg root=/dev/mapper/vg-lv_root quiet rw\"" \
    /etc/default/grub

# a hack to work around a grub-mkconfig bug with lvm2
# https://bbs.archlinux.org/viewtopic.php?id=242594
ln -s /hostlvm /run/lvm
grub-mkconfig -o /boot/grub/grub.cfg

# add user
useradd -m -G wheel -s /bin/bash $USER_NAME
echo $USER_NAME:$USER_PASSWORD | chpasswd

# install more software
pacman -S --noconfirm git vim emacs tlp networkmanager

# enable systemd daemons
systemctl enable NetworkManager
systemctl enable tlp tlp-sleep
systemctl enable fstrim.timer

# exit chroot
exit

