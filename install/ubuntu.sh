#!/bin/bash

DISK=/dev/vda
HOSTNAME=mulberry
ROOT_PASSWD=root
IWLWIFI_FIRMWARE=iwlwifi-8265-36.ucode.xz
# IWLWIFI_FIRMWARE=iwlwifi-QuZ-a0-hr-b0-72.ucode.xz

# disable selinux
#setenforce 0

sfdisk $DISK <<< "
    label: gpt
    size=512M,type=U,bootable
    size=+,type=L
"

# partition ids (actually PARTUUID)
PART1_UUID=$(sfdisk -q --part-uuid $DISK 1)
PART2_UUID=$(sfdisk -q --part-uuid $DISK 2)

PART1=$(sfdisk -q -l $DISK -o Device,UUID | grep -i $PART1_UUID | cut -d' ' -f1)
PART2=$(sfdisk -q -l $DISK -o Device,UUID | grep -i $PART2_UUID | cut -d' ' -f1)

# create file systems
mkfs.fat -F32 $PART1
mkfs.btrfs -f $PART2

# create btrfs subvolumes
mount $PART2 /mnt
cd /mnt
btrfs subvolume create @
btrfs subvolume create @home
btrfs subvolume create @var
cd
umount /mnt

# mount btrfs subvolumes
mount -o compress=zstd,subvol=@ $PART2 /mnt
mkdir -p /mnt/{home,var}
mount -o compress=zstd,subvol=@home $PART2 /mnt/home
mount -o compress=zstd,subvol=@var $PART2 /mnt/var
mkdir -p /mnt/boot/efi
mount $PART1 /mnt/boot/efi

# mount
mkdir -p /mnt/{proc,sys,dev/pts}
mount -t proc proc /mnt/proc
mount -t sysfs sys /mnt/sys
mount -B /dev /mnt/dev
mount -t devpts pts /mnt/dev/pts

# bootstrap minimal system
cat > /etc/apt/apt.conf.d/01norecommend << EOF
APT::Install-Recommends "0";
APT::Install-Suggests "0";
EOF

source /etc/os-release
apt update
apt install debootstrap

debootstrap $UBUNTU_CODENAME /mnt

cp /etc/apt/sources.list /mnt/etc/apt/sources.list

# use host's resolv.conf
cp /etc/resolv.conf /mnt/etc/resolv.conf

# generate /etc/fstab
PART1_UUID=$(blkid $PART1 -s UUID -o value)
PART2_UUID=$(blkid $PART2 -s UUID -o value)
cat > /mnt/etc/fstab << EOF
UUID=$PART1_UUID /boot/efi vfat  umask=0077                       0 2
UUID=$PART2_UUID /         btrfs compress-force=zstd,subvol=@     0 0
UUID=$PART2_UUID /home     btrfs compress-force=zstd,subvol=@home 0 0
UUID=$PART2_UUID /var      btrfs compress-force=zstd,subvol=@var  0 0
EOF

cat << EOF | chroot /mnt /bin/bash
mount -a

apt update


# install essential programs
cat > /etc/apt/apt.conf.d/99norecommends << EOF
apt install -y btrfs-progs       \
               cracklib-dicts    \
               doas              \
               efibootmgr        \
               iwd               \
               systemd-boot      \
               systemd-networkd  \
               vim

bootctl install --efi-boot-option-description="Ubuntu"
apt install -y linux-image-generic

# keep the needed iwlwifi_firmware only
cp /lib/firmware/$IWLWIFI_FIRMWARE /tmp/
dnf remove -y iwl{7260,ax2xx}-firmware
cp /tmp/$IWLWIFI_FIRMWARE /lib/firmware/

systemd-firstboot               \
    --locale="en_US.UTF-8"      \
    --keymap="us"               \
    --timezone="Canada/Eastern" \
    --hostname="$HOSTNAME"

echo "root:$ROOT_PASSWD" | chpasswd
passwd --expire root

# done
exit
EOF

# umount
umount -n -R /mnt

# ready to reboot
echo ""
echo "Installation is now complete!"
echo "Type $(tput setaf 2)reboot$(tput sgr0) to restart the machine."
echo ""
