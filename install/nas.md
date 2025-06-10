# NAS Installation Guide

## Preparation
1. download the Red Hat Enterprise Linux 10.0 Boot ISO
2. burn the iso to usb key
```bash
sudo dd bs=4M if=/path/to/rhel-10.0-x86_64-boot.iso of=/dev/sdx status=progress && sync
```
## System Installation
1. name the system **nasberry**
2. partition the ssd as follows

| Size     | Mount Point | Type            |
|----------|-------------|-----------------|
| 200M     | /boot/efi   | EFI File System |
| NVME SSD | /           | xfs             |
| SATA SSD | /data       | xfs             |

3. create an administrative account: **peter**
4. create a service account: **bee**
```bash
sudo useradd -r -s /sbin/nologin bee
```
5. do not install weak dependencies
```bash
echo "install_weak_deps=False" | sudo tee --append /etc/dnf/dnf.conf
```
6. use static IP
```bash
sudo nmcli con modify enp1s0 ipv4.method manual
sudo nmcli con modify enp1s0 ipv4.addresses 192.168.0.254/24
sudo nmcli con modify enp1s0 ipv4.gateway 192.168.0.1
sudo nmcli con modify enp1s0 ipv4.dns 192.168.0.1

sudo nmcli connection down enp1s0
sudo nmcli connection up   enp1s0
```

## Configure SELinux
1. preparation
```bash
echo "%wheel    ALL=(ALL)   ROLE=sysadm_r   NOPASSWD:ALL" |  \
sudo EDITOR="tee --append" visudo -f /etc/sudoers.d/administrators

sudo dnf install policycoreutils-python-utils \
                 policycoreutils-restorecond  \
                 selinux-policy-devel         \
                 setroubleshoot-server        \
                 setools-console

# allow ssh access for sysadm_u
sudo setsebool -P ssh_sysadm_login on

# https://tinyurl.com/2ay788my
# https://access.redhat.com/articles/3263671
sudo semanage login -m -s user_u -r s0 __default__
sudo usermod -Z sysadm_u peter
sudo restorecon -RFv /home/peter
```
2. create a customer policy
```bash
# create a directory to host the files
mkdir -p ~/semodules/nasberry

# enter the directory
cd ~/semodules/nasberry

# create type enforcement file
cat << EOF > nasberry.te
policy_module(nasberry, 1.0)

# types
require {
    type container_runtime_t;
    type samba_share_t;
    type semanage_store_t;
    type sysadm_sudo_t;
    type sysadm_t;
}

# classes
require {
    class dir search;
    class process getpgid;
    class unix_stream_socket { getattr ioctl create setopt };
}

# allowances
allow sysadm_sudo_t semanage_store_t:dir search;

allow sysadm_sudo_t sysadm_t:process getpgid;
allow sysadm_sudo_t sysadm_t:unix_stream_socket { getattr ioctl };

allow sysadm_t container_runtime_t:unix_stream_socket { create setopt };
EOF

# create a file context file
cat << EOF > nasberry.fc
/data/share(/.*)?  gen_context(system_u:object_r:samba_share_t,s0)
EOF

# build the policy
sudo make -f /usr/share/selinux/devel/Makefile nasberry.pp

# remove the policy if already loaded
sudo semodule -r nasberry

# install the policy
sudo semodule -i nasberry.pp
```

## Configure Virtualization
1. install **libvirtd** service
```bash
sudo dnf install libvirt
sudo systemctl enable --now libvirtd
sudo reboot
```

## Install cockpit
```bash
sudo dnf install cockpit{,-{files,machines,podman,storaged}}
sudo dnf install tuned

# enable VNC access
sudo firewall-cmd --permanent --add-port 5900/tcp
sudo firewall-cmd --reload
```

## Configure zram
```bash
sudo dnf install zram-generator
sudo tee /etc/systemd/zram-generator.conf << EOF
[zram0]
zram-size = min(ram / 2, 4096)
EOF
```

## Configure Ollama + Open WebUI
1. create relevant directories
```bash
sudo mkdir -p /srv/ollama-oi/{ollama,data}
```
2. open ports on firewall
```bash
sudo firewall-cmd --permanent --add-port 53000/tcp

sudo firewall-cmd --reload
```
3. create quadlet for container
```bash
sudo tee /etc/containers/systemd/ollama-oi.container << EOF
[Unit]
Description=ollama-oi

[Container]
Image=ghcr.io/open-webui/open-webui:ollama

Volume=/srv/ollama-oi/data:/app/backend/data:z
Volume=/srv/ollama-oi/ollama:/root/.ollama:z

Environment=WEBUI_AUTH=False

PublishPort=53000:8080

[Install]
WantedBy=multi-user.target
EOF
```
4. generate the systemd service
```bash
sudo systemctl daemon-reload
```
5. start the ollama-oi service
```bash
sudo systemctl start ollama-oi.service
```

## Configure qBittorrent
1. create **downloads** directory
```bash
sudo mkdir -p /srv/qbittorrent/downloads
sudo chown -R bee:bee /srv/qbittorrent/downloads
```
2. create **config** directory and default config
```bash
sudo mkdir -p /srv/qbittorrent/config/qBittorrent/config
sudo tee /srv/qbittorrent/config/qBittorrent/config/qBittorrent.conf << EOF
[BitTorrent]
Session\AddTorrentStopped=true
Session\DefaultSavePath=/downloads
Session\Port=56881
Session\TempPath=/downloads/temp

[Preferences]
General\Locale=en
WebUI\AuthSubnetWhitelist=192.168.0.0/24
WebUI\AuthSubnetWhitelistEnabled=true
WebUI\LocalHostAuth=false
EOF

sudo chown -R bee:bee /srv/qbittorrent/config
```
3. open ports on firewall
```bash
sudo firewall-cmd --permanent --add-port 58080/tcp
sudo firewall-cmd --permanent --add-port 56881/tcp
sudo firewall-cmd --permanent --add-port 56881/udp

sudo firewall-cmd --reload
```
4. create quadlet for container
```bash
sudo tee /etc/containers/systemd/qbittorrent.container << EOF
[Unit]
Description=qbittorrent-nox

[Container]
Image=docker.io/qbittorrentofficial/qbittorrent-nox:latest

Volume=/srv/qbittorrent/config:/config:z
Volume=/srv/qbittorrent/downloads:/downloads:z

Environment=PUID=$(id -u bee)
Environment=PGID=$(id -g bee)
Environment=TZ=America/Toronto
Environment=TORRENTING_PORT=56881
Environment=QBT_LEGAL_NOTICE=confirm
Environment=QBT_WEBUI_PORT=58080

PublishPort=58080:58080
PublishPort=56881:56881
PublishPort=56881:56881/udp

[Install]
WantedBy=multi-user.target
EOF
```
5. generate the systemd service
```bash
sudo systemctl daemon-reload
```
6. start the qbittorrent service
```bash
sudo systemctl start qbittorrent.service
```
7. go to cockpit to find the initial password in the running container's log

## Configure Samba
1. install samba
```bash
sudo dnf install samba
```
2. set a samba password
```bash
sudo smbpasswd -a peter
```
3. create directories
```bash
sudo mkdir -p /data/share
sudo chown -R peter:peter /data/share
sudo restorecon -RFv /data/share
```
4. configure firewall
```bash
sudo firewall-cmd --permanent --add-service samba
sudo firewall-cmd --reload
```
5. edit /etc/samba/smb.conf
```bash
sudo tee /etc/samba/smb.conf << EOF
[global]
   workgroup = HOME
   server string = Samba Server %v
   security = user
   map to guest = Bad User
   dns proxy = no
   log file = /var/log/samba/log.%m
   max log size = 1000

[Share]
   comment = Share
   path = /data/share
   browsable = yes
   writable = no
   read only = yes
   public = no
   valid users = peter
EOF
```
6. test and enable samba
```bash
sudo testparm
sudo systemctl enable --now smb nmb
```
## Remove unused packages and residues
```bash
sudo dnf remove rhc insights-client

sudo userdel yggdrasil
sudo userdel yggdrasil-worker

sudo groupdel yggdrasil
sudo groupdel yggdrasil-worker
```
