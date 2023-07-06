#!/bin/bash

DISK=/dev/vda
HOSTNAME=mulberry
ROOT_PASSWD=root
IWLWIFI_FIRMWARE=iwlwifi-8265-36.ucode
# IWLWIFI_FIRMWARE=iwlwifi-QuZ-a0-hr-b0-72.ucode

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
mount -o compress-force=zstd,subvol=@ $PART2 /mnt
mkdir -p /mnt/{home,var}
mount -o compress-force=zstd,subvol=@home $PART2 /mnt/home
mount -o compress-force=zstd,subvol=@var $PART2 /mnt/var
mkdir -p /mnt/boot/efi
mount $PART1 /mnt/boot/efi

# for dir in sys dev proc; do
#     mount --rbind $dir /mnt/$dir
#     mount --make-rslave /mnt/$dir
# done

# bootstrap minimal system
cat > /etc/apt/apt.conf.d/01norecommends << EOF
APT::Install-Recommends "0";
APT::Install-Suggests "0";
EOF

source /etc/os-release
apt update
apt install -y debootstrap

debootstrap $UBUNTU_CODENAME /mnt

# mount
mkdir -p /mnt/{proc,sys,dev/pts}
mount -t proc proc /mnt/proc
mount -t sysfs sys /mnt/sys
mount -B /dev /mnt/dev
mount -t devpts pts /mnt/dev/pts

# save the iwlwifi_firmware on the host
mkdir -p /mnt/lib/firmware
cp /lib/firmware/$IWLWIFI_FIRMWARE /mnt/lib/firmware/

# use complete sources.list
cat > /mnt/etc/apt/sources.list << EOF
deb http://archive.ubuntu.com/ubuntu/ $UBUNTU_CODENAME main restricted universe multiverse
# deb-src http://archive.ubuntu.com/ubuntu/ $UBUNTU_CODENAME restricted universe multiverse

deb http://archive.ubuntu.com/ubuntu/ $UBUNTU_CODENAME-updates main restricted universe multiverse
# deb-src http://archive.ubuntu.com/ubuntu/ $UBUNTU_CODENAME-updates main restricted universe multiverse

deb http://archive.ubuntu.com/ubuntu/ $UBUNTU_CODENAME-security main restricted universe multiverse
# deb-src http://archive.ubuntu.com/ubuntu/ $UBUNTU_CODENAME-security main restricted universe multiverse

deb http://archive.ubuntu.com/ubuntu/ $UBUNTU_CODENAME-backports main restricted universe multiverse
# deb-src http://archive.ubuntu.com/ubuntu/ $UBUNTU_CODENAME-backports main restricted universe multiverse
EOF

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

# don't install recommended programs by default
echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/99norecommends

# install essential programs
apt install -y doas         \
               iwd          \
               systemd-boot \
               vim

bootctl install --esp-path=/boot/efi --efi-boot-option-description="Ubuntu"
apt install -y linux-image-generic

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
