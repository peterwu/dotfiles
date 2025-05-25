#!/usr/bin/env bash

SUITE=trixie
DISK0=/dev/vda
DISK1=/dev/vdb

PASSWD_FOR_ROOT=root
PASSWD_FOR_PETER=peter

HOSTNAME=debian

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
wget https://mirrors.kernel.org/pub/linux/utils/kbd/kbd-2.7.1.tar.gz -O /tmp/kbd-2.7.1.tar.gz
tar xzf /tmp/kbd-2.7.1.tar.gz -C /tmp
mkdir -p /mnt/usr/share/keymaps
cp -Rp /tmp/kbd-2.7.1/data/keymaps/* /mnt/usr/share/keymaps/

# chroot
cat << END_OF_CHROOT | chroot /mnt /bin/bash

set -e

# set apt sources
cat << EOF > /etc/apt/sources.list
deb http://deb.debian.org/debian               $SUITE           main contrib non-free non-free-firmware
deb http://deb.debian.org/debian               $SUITE-updates   main contrib non-free non-free-firmware
deb http://deb.debian.org/debian               $SUITE-backports main contrib non-free non-free-firmware
deb http://security.debian.org/debian-security $SUITE-security  main contrib non-free non-free-firmware
EOF

# don't install recommends
cat << EOF > /etc/apt/apt.conf.d/99norecommends
APT::Install-Recommends "false";
EOF

apt update
apt dist-upgrade --yes

# install kernel and grub
apt install --yes linux-image-amd64 firmware-linux zstd     \
                  grub-pc grub-efi-amd64-signed shim-signed \
                  systemd-resolved systemd-zram-generator

grub-install --efi-directory=/boot/efi --bootloader-id="Debian" --uefi-secure-boot $DISK0
update-grub

# set locale
apt install --yes locales

cat << EOF >> /etc/locale.gen
en_US.UTF-8 UTF-8
EOF

locale-gen

cat << EOF > /etc/locale.conf
LANG=en_US.UTF-8
EOF

# set timezone
ln -sf /usr/share/zoneinfo/America/Toronto /etc/localtime

# set hostname
echo $HOSTNAME > /etc/hostname

# set keymap
echo "KEYMAP=us" > /etc/vconsole.conf

# add user
useradd -m peter -G sudo

# set passwords
cat << EOF | chpasswd
root:$PASSWD_FOR_ROOT
peter:$PASSWD_FOR_PETER
EOF

# require immediate change of password upon first login
passwd --expire root
passwd --expire peter

# replace ifupdown with systemd-networkd
cat << EOF > /etc/systemd/network/20-wired.network
[Match]
Name=enp1s0

[Network]
DHCP=yes
EOF

systemctl enable systemd-networkd
systemctl enable systemd-resolved

rm -rf /etc/network
apt purge --yes ifupdown

# install apparmor
apt install apparmor apparmor-utils auditd
cat << EOF > /etc/default/grub.d/apparmor.cfg
GRUB_CMDLINE_LINUX_DEFAULT="$GRUB_CMDLINE_LINUX_DEFAULT apparmor=1 security=apparmor"
EOF

# remove unwanted apps
apt purge --yes nano

# exit chroot
exit

END_OF_CHROOT

umount -R /mnt

read -p "Press enter to reboot ..."
reboot

