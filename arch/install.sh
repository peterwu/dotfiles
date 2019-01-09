#!/bin/bash

# shortened url
# https://bit.ly/2TD7QzE
# curl -L https://bit.ly/2TD7QzE -o install.sh
# curl -L https://bit.ly/2SMSits -o chroot.sh

# chmod +x install.sh
# ./install.sh


# prep the wifi connection
WIFI_AP_NAME=ssid
WIFI_PASSWORD=password
# nmcli device wifi connect $WIFI_AP_NAME password $WIFI_PASSWORD

### run lsblk to find out the disk ###
DISK=/dev/sda
LUKS_PASSPHRASE=passphrase
CLOSEST_MIRROR='Server = https://mirror.csclub.uwaterloo.ca/archlinux/$repo/os/$arch'

# installation begins
echo 'installation begins'

# set the ntp
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
pacstrap /mnt base base-devel

# fstab
genfstab -U /mnt > /mnt/etc/fstab

## a hack to work around a grub-mkconfig bug with lvm2
# https://bbs.archlinux.org/viewtopic.php?id=242594
mkdir /mnt/hostlvm
mount --bind /run/lvm /mnt/hostlvm

# chroot
cp chroot.sh /mnt
arch-chroot /mnt /bin/bash chroot.sh
umount -R /mnt

# done, asking user to reboot
echo "installation completed. type reboot to restart the computer."
