#!/bin/bash

# variables
WIFI_AP_NAME=ssid
WIFI_PASSWORD=password
### run lsblk to find out the disk ###
DISK=/dev/sda
LUKS_PASSPHRASE=passphrase
CLOSEST_MIRROR=https://mirror.csclub.uwaterloo.ca/archlinux/$repo/os/$arch
HOST_NAME=boysenberry
ROOT_PASSWORD=root_password
USER_NAME=peter
USER_PASSWORD=user_password

# prep the wifi connection
nmcli device wifi connect $WIFI_AP_NAME password $WIFI_PASSWORD
timedatectl set-ntp true

# disk partition
parted --script $DISK \
  mklabel gpt \
  mkpart primary fat32 1MiB 501MiB \
  set 1 esp on \
  mkpart primary ext4 501MiB 100%

# luks encryption
echo $LUKS_PASSPHRASE | cryptsetup luksFormat $DISK"2"
echo $LUKS_PASSPHRASE | cryptsetup open --type luks $DISK"2" lvm

# create lvm
pvcreate /dev/mapper/lvm
vgcreate vg /dev/mapper/lvm
lvcreate -L 2G vg -n lv_swap
lvcreate -L 15G vg -n lv_root
lvcreate -l 100%FREE vg -n lv_home

vgscan
vgchange -ay

# format partitions
mkfs.fat -F32 $DISK"1"
mkfs.ext4 /dev/mapper/vg-lv_root
mkfs.ext4 /dev/mapper/vg-lv_home

mkswap /dev/mapper/vg-lv_swap
swapon /dev/mapper/vg-lv_swap

# mounting file system
mount /dev/mapper/vg-lv_root /mnt
mkdir /mnt/home
mount /dev/mapper/vg-lv_home /mnt/home
mkdir /mnt/boot
mount /dev/sda1 /mnt/boot

# install the base system
echo $CLOSEST_MIRROR > /etc/pacman.d/mirrorlist
pacstrap -i /mnt base base-devel

# fstab
genfstab -U /mnt > /mnt/etc/fstab

# chroot
arch-chroot /mnt /bin/bash
localectl set-locale LANG=en_CA.UTF-8
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
grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=grub 
grub-mkfont -s 32 -o /boot/grub/fonts/font.pf2 /usr/share/fonts/misc/ter-x32n.pcf.gz

# grub
### TODO: read ! blkid /dev/sda2 ###
UUID=$(cut -d " " -f 2 | cut -d "=" -f 2 | cut -d '"' -f 2)
sed -i.bak \
    -e '/^GRUB_CMDLINE_LINUX_DEFAULT=/c GRUB_CMDLINE_LINUX_DEFAULT=cryptdevice=UUID=$UUID:vg splash quiet' \
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

# clean up and reboot
exit
umount -R /mnt

# done, asking user to reboot
echo "installation completed. type reboot to restart the computer."
