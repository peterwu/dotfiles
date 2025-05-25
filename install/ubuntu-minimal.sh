#!/usr/bin/env bash

SUITE=noble
DISK0=/dev/vda
DISK1=/dev/vdb

PASSWD_FOR_ROOT=root
PASSWD_FOR_PETER=peter

HOSTNAME=ubuntu

set -x

# check if running as root
if [ "$(id -u)" -ne 0 ]; then
    echo "ERROR: This script must be run as root."
    exit 1
fi

# install necessary packages
apt update && apt install --yes debootstrap arch-install-scripts

# partition disks
# DISK0
parted -s $DISK0 mklabel gpt
parted -s $DISK0 mkpart primary fat32 1MiB 200MiB

parted -s $DISK0 set 1 boot on
parted -s $DISK0 set 1 esp on

parted -s $DISK0 mkpart primary ext4 200MiB 100%

partprobe $DISK0
sleep 2

# DISK1
parted -s $DISK1 mklabel gpt
parted -s $DISK1 mkpart primary ext4 1MiB 100%

partprobe $DISK1
sleep 2

EFI_DEV="${DISK0}1"
ROOT_DEV="${DISK0}2"
DATA_DEV="${DISK1}1"

# mkfs
mkfs.vfat -F32 $EFI_DEV
mkfs.ext4 -F $ROOT_DEV
mkfs.ext4 -F $DATA_DEV

# mounts
mount $ROOT_DEV /mnt
mkdir -p /mnt/boot/efi
mount $EFI_DEV /mnt/boot/efi
mkdir -p /mnt/data
mount $DATA_DEV /mnt/data

# bootstrap
debootstrap $SUITE /mnt

# mounts
for dir in dev proc sys; do
    mount --rbind /$dir /mnt/$dir
    mount --make-rslave /mnt/$dir
done

# genfstab
genfstab -U /mnt >> /mnt/etc/fstab

# https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=790955
wget https://mirrors.edge.kernel.org/pub/linux/utils/kbd/kbd-2.7.1.tar.gz -O /tmp/kbd-2.7.1.tar.gz
tar xzf /tmp/kbd-2.7.1.tar.gz -C /tmp
mkdir -p /mnt/usr/share/keymaps
cp -Rp /tmp/kbd-2.7.1/data/keymaps/* /mnt/usr/share/keymaps/

# chroot
cat << END_OF_FILE | chroot /mnt /bin/bash

set -e

cat << EOF > /etc/apt/sources.list
deb http://archive.ubuntu.com/ubuntu $SUITE           main restricted universe multiverse
deb http://archive.ubuntu.com/ubuntu $SUITE-updates   main restricted universe multiverse
deb http://archive.ubuntu.com/ubuntu $SUITE-backports main restricted universe multiverse
deb http://archive.ubuntu.com/ubuntu $SUITE-security  main restricted universe multiverse
EOF

cat << EOF > /etc/apt/apt.conf.d/99norecommends
APT::Install-Recommends "false";
EOF

apt update
apt dist-upgrade --yes

systemd-firstboot                \
    --keymap="us"                \
    --timezone="America/Toronto" \
    --hostname="$HOSTNAME"

locale-gen --purge en_US.UTF-8

apt install --yes linux-generic linux-firmware      \
                  grub-efi-amd64-signed shim-signed \
                  systemd-zram-generator

grub-install --efi-directory=/boot/efi --bootloader-id="Ubuntu" --uefi-secure-boot $DISK0
update-grub

useradd -m peter -G sudo

cat << EOF | chpasswd
root:$PASSWD_FOR_ROOT
peter:$PASSWD_FOR_PETER
EOF

passwd --expire root
passwd --expire peter

cat << EOF > /etc/systemd/network/20-wired.network
[Match]
Name=enp1s0

[Network]
DHCP=yes
EOF

systemctl enable systemd-networkd
systemctl enable systemd-resolved

exit

END_OF_FILE

umount -R /mnt

read -p "Press enter to reboot ..."
reboot

