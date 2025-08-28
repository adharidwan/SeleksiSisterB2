## Deskripsi Topologi

Topologi terdiri dari 4 negara: **Gokouloryn**, **Rurinthia**, **Kuronexus**, dan **Yamindralia**. Setiap negara memiliki struktur yang sama dengan 3 zona: Government, Enterprise, dan Public Zone.

## 1. Perancangan Topologi Dasar (2.5 poin)

### Perangkat yang Dibutuhkan:

#### Per Negara:
- 1 Border Router (Router 1941, kecuali Kuronexus menggunakan Router 2911)
- 3 Internal Router (Router 1941) untuk masing-masing zona
- 4 Switch (untuk menghubungkan border router ke internal router, dan satu di setiap zona)
- 1 PC di Government Zone
- 1 PC di Public Zone (kecuali Kuronexus yang khusus)
- 1 PC, 1 Web Server, 1 DNS Server, 1 DHCP Server di Enterprise Zone

#### Eksternal:
- 1 Switch untuk menghubungkan border router Gokouloryn, Rurinthia, dan Kuronexus

### Skema Penamaan:
Format: `X_Nama` dimana X = nama negara
- Gokouloryn: G_BorderRouter, G_GovRouter, G_EntRouter, G_PubRouter, dll
- Rurinthia: R_BorderRouter, R_GovRouter, R_EntRouter, R_PubRouter, dll  
- Kuronexus: K_BorderRouter, K_GovRouter, K_EntRouter, K_PubRouter, dll
- Yamindralia: Y_BorderRouter, Y_GovRouter, Y_EntRouter, Y_PubRouter, dll

## KONEKSI INTERFACE DETAIL UNTUK SEMUA NEGARA

### GOKOULORYN (10.1.x.x/24)

#### G_BorderRouter (Router 1941):
```cisco
enable
configure terminal
hostname G_BorderRouter
interface gigabit0/0
ip address 192.168.100.1 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.1.0.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → External_Switch (port fa0/1) - Menghubungkan ke eksternal
- **Fa0/1** → G_Internal_Switch (port fa0/1) - Menghubungkan ke switch internal Gokouloryn

#### G_GovRouter (Router 1941):
```cisco
enable
configure terminal
hostname G_GovRouter
interface fa0/0
ip address 10.1.0.2 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.1.1.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → G_Internal_Switch (port fa0/2) - Menghubungkan ke switch internal
- **Fa0/1** → G_Gov_Switch (port fa0/1) - Menghubungkan ke switch Government Zone

#### G_EntRouter (Router 1941):
```cisco
enable
configure terminal
hostname G_EntRouter
interface fa0/0
ip address 10.1.0.3 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.1.2.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → G_Internal_Switch (port fa0/3) - Menghubungkan ke switch internal
- **Fa0/1** → G_Ent_Switch (port fa0/1) - Menghubungkan ke switch Enterprise Zone

#### G_PubRouter (Router 1941):
```cisco
enable
configure terminal
hostname G_PubRouter
interface fa0/0
ip address 10.1.0.4 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.1.3.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → G_Internal_Switch (port fa0/4) - Menghubungkan ke switch internal
- **Fa0/1** → G_Pub_Switch (port fa0/1) - Menghubungkan ke switch Public Zone

### RURINTHIA (10.2.x.x/24)

#### R_BorderRouter (Router 1941):
```cisco
enable
configure terminal
hostname R_BorderRouter
interface fa0/0
ip address 192.168.100.2 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.2.0.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → External_Switch (port fa0/2) - Menghubungkan ke eksternal
- **Fa0/1** → R_Internal_Switch (port fa0/1) - Menghubungkan ke switch internal Rurinthia

#### R_GovRouter (Router 1941):
```cisco
enable
configure terminal
hostname R_GovRouter
interface fa0/0
ip address 10.2.0.2 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.2.1.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → R_Internal_Switch (port fa0/2) - Menghubungkan ke switch internal
- **Fa0/1** → R_Gov_Switch (port fa0/1) - Menghubungkan ke switch Government Zone

#### R_EntRouter (Router 1941):
```cisco
enable
configure terminal
hostname R_EntRouter
interface fa0/0
ip address 10.2.0.3 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.2.2.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → R_Internal_Switch (port fa0/3) - Menghubungkan ke switch internal
- **Fa0/1** → R_Ent_Switch (port fa0/1) - Menghubungkan ke switch Enterprise Zone

#### R_PubRouter (Router 1941):
```cisco
enable
configure terminal
hostname R_PubRouter
interface fa0/0
ip address 10.2.0.4 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.2.3.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → R_Internal_Switch (port fa0/4) - Menghubungkan ke switch internal
- **Fa0/1** → R_Pub_Switch (port fa0/1) - Menghubungkan ke switch Public Zone

### KURONEXUS (10.3.x.x/24)

#### K_BorderRouter (Router 2911):
```cisco
enable
configure terminal
hostname K_BorderRouter
interface fa0/0
ip address 192.168.100.3 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 192.168.200.1 255.255.255.0
no shutdown
exit
interface fa1/0
ip address 10.3.0.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → External_Switch (port fa0/3) - Menghubungkan ke eksternal
- **Fa0/1** → Y_BorderRouter Fa0/0 (direct connection) - Menghubungkan langsung ke Yamindralia
- **Fa1/0** → K_Internal_Switch (port fa0/1) - Menghubungkan ke switch internal Kuronexus

#### K_GovRouter (Router 1941):
```cisco
enable
configure terminal
hostname K_GovRouter
interface fa0/0
ip address 10.3.0.2 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.3.1.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → K_Internal_Switch (port fa0/2) - Menghubungkan ke switch internal
- **Fa0/1** → K_Gov_Switch (port fa0/1) - Menghubungkan ke switch Government Zone

#### K_EntRouter (Router 1941):
```cisco
enable
configure terminal
hostname K_EntRouter
interface fa0/0
ip address 10.3.0.3 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.3.2.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → K_Internal_Switch (port fa0/3) - Menghubungkan ke switch internal
- **Fa0/1** → K_Ent_Switch (port fa0/1) - Menghubungkan ke switch Enterprise Zone

#### K_PubRouter (Router 1941) - Router-on-a-Stick untuk VLAN:
```cisco
enable
configure terminal
hostname K_PubRouter
interface fa0/0
ip address 10.3.0.4 255.255.255.0
no shutdown
exit
interface fa0/1
no shutdown
exit
interface fa0/1.30
encapsulation dot1Q 30
ip address 10.3.3.1 255.255.255.0
exit
interface fa0/1.40
encapsulation dot1Q 40
ip address 10.3.4.1 255.255.255.0
exit
interface fa0/1.50
encapsulation dot1Q 50
ip address 10.3.5.1 255.255.255.0
exit
```
**KONEKSI:**
- **Fa0/0** → K_Internal_Switch (port fa0/4) - Menghubungkan ke switch internal
- **Fa0/1** → K_Pub_Switch (port fa0/24 as trunk) - Menghubungkan ke switch Public Zone (trunk untuk VLAN)

### YAMINDRALIA (10.4.x.x/24)

#### Y_BorderRouter (Router 1941):
```cisco
enable
configure terminal
hostname Y_BorderRouter
interface fa0/0
ip address 192.168.200.2 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.4.0.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → K_BorderRouter Fa0/1 (direct connection) - Menghubungkan langsung ke Kuronexus
- **Fa0/1** → Y_Internal_Switch (port fa0/1) - Menghubungkan ke switch internal Yamindralia

#### Y_GovRouter (Router 1941):
```cisco
enable
configure terminal
hostname Y_GovRouter
interface fa0/0
ip address 10.4.0.2 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.4.1.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → Y_Internal_Switch (port fa0/2) - Menghubungkan ke switch internal
- **Fa0/1** → Y_Gov_Switch (port fa0/1) - Menghubungkan ke switch Government Zone

#### Y_EntRouter (Router 1941):
```cisco
enable
configure terminal
hostname Y_EntRouter
interface fa0/0
ip address 10.4.0.3 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.4.2.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → Y_Internal_Switch (port fa0/3) - Menghubungkan ke switch internal
- **Fa0/1** → Y_Ent_Switch (port fa0/1) - Menghubungkan ke switch Enterprise Zone

#### Y_PubRouter (Router 1941):
```cisco
enable
configure terminal
hostname Y_PubRouter
interface fa0/0
ip address 10.4.0.4 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.4.3.1 255.255.255.0
no shutdown
exit
```
**KONEKSI:**
- **Fa0/0** → Y_Internal_Switch (port fa0/4) - Menghubungkan ke switch internal
- **Fa0/1** → Y_Pub_Switch (port fa0/1) - Menghubungkan ke switch Public Zone

## KONEKSI PERANGKAT END DEVICES

### GOKOULORYN DEVICES:

#### G_Gov_Switch:
- **Port Fa0/1** → G_GovRouter Fa0/1
- **Port Fa0/2** → G_Gov_PC

#### G_Ent_Switch:
- **Port Fa0/1** → G_EntRouter Fa0/1
- **Port Fa0/2** → G_Web_Server (IP: 10.1.2.10)
- **Port Fa0/3** → G_DNS_Server (IP: 10.1.2.11)
- **Port Fa0/4** → G_DHCP_Server (IP: 10.1.2.12)
- **Port Fa0/5** → G_Ent_PC

#### G_Pub_Switch:
- **Port Fa0/1** → G_PubRouter Fa0/1
- **Port Fa0/2** → G_Pub_PC1 (untuk NAT testing, IP statis genap)
- **Port Fa0/3** → G_Pub_PC2 (untuk NAT testing, IP statis genap)

### RURINTHIA DEVICES:

#### R_Gov_Switch:
- **Port Fa0/1** → R_GovRouter Fa0/1
- **Port Fa0/2** → R_Gov_PC

#### R_Ent_Switch:
- **Port Fa0/1** → R_EntRouter Fa0/1
- **Port Fa0/2** → R_Web_Server (IP: 10.2.2.10)
- **Port Fa0/3** → R_DNS_Server (IP: 10.2.2.11)
- **Port Fa0/4** → R_DHCP_Server (IP: 10.2.2.12)
- **Port Fa0/5** → R_Ent_PC

#### R_Pub_Switch:
- **Port Fa0/1** → R_PubRouter Fa0/1
- **Port Fa0/2** → R_Pub_PC

### KURONEXUS DEVICES:

#### K_Gov_Switch:
- **Port Fa0/1** → K_GovRouter Fa0/1
- **Port Fa0/2** → K_Gov_PC

#### K_Ent_Switch:
- **Port Fa0/1** → K_EntRouter Fa0/1
- **Port Fa0/2** → K_Web_Server (IP: 10.3.2.10)
- **Port Fa0/3** → K_DNS_Server (IP: 10.3.2.11)
- **Port Fa0/4** → K_DHCP_Server (IP: 10.3.2.12)
- **Port Fa0/5** → K_Ent_PC

#### K_Pub_Switch (VLAN Configured):
- **Port Fa0/24** → K_PubRouter Fa0/1 (TRUNK for VLANs)
- **VLAN 30 (Academy):**
  - **Port Fa0/1-5** → K_Academy_PC1, K_Academy_PC2, etc.
- **VLAN 40 (Business):**
  - **Port Fa0/6-10** → K_Business_PC1, K_Business_PC2, etc.
- **VLAN 50 (Communal - Wireless):**
  - **Port Fa0/11** → K_Wireless_AccessPoint
  - **Wireless:** K_Smartphone, K_Laptop_Wireless

### YAMINDRALIA DEVICES:

#### Y_Gov_Switch:
- **Port Fa0/1** → Y_GovRouter Fa0/1
- **Port Fa0/2** → Y_Gov_PC

#### Y_Ent_Switch:
- **Port Fa0/1** → Y_EntRouter Fa0/1
- **Port Fa0/2** → Y_Web_Server (IP: 10.4.2.10)
- **Port Fa0/3** → Y_DNS_Server (IP: 10.4.2.11)
- **Port Fa0/4** → Y_DHCP_Server (IP: 10.4.2.12)
- **Port Fa0/5** → Y_Ent_PC

#### Y_Pub_Switch:
- **Port Fa0/1** → Y_PubRouter Fa0/1
- **Port Fa0/2** → Y_Pub_PC

### ALOKASI IP ADDRESS LENGKAP:

#### GOKOULORYN (10.1.x.x/24):
- **Border Router:**
  - Fa0/0 (ke eksternal): 192.168.100.1/24
  - Fa0/1 (ke internal switch): 10.1.0.1/24
- **Government Zone: 10.1.1.0/24**
  - Router: 10.1.1.1/24
  - PC: DHCP (range 10.1.1.11-10.1.1.100)
- **Enterprise Zone: 10.1.2.0/24**
  - Router: 10.1.2.1/24
  - Web Server: 10.1.2.10/24
  - DNS Server: 10.1.2.11/24
  - DHCP Server: 10.1.2.12/24
  - PC: DHCP (range 10.1.2.21-10.1.2.100)
- **Public Zone: 10.1.3.0/24**
  - Router: 10.1.3.1/24
  - PC1: 10.1.3.2/24 (static - untuk NAT testing)
  - PC2: 10.1.3.4/24 (static - untuk NAT testing)

#### RURINTHIA (10.2.x.x/24):
- **Border Router:**
  - Fa0/0 (ke eksternal): 192.168.100.2/24
  - Fa0/1 (ke internal switch): 10.2.0.1/24
- **Government Zone: 10.2.1.0/24**
  - Router: 10.2.1.1/24
  - PC: DHCP (range 10.2.1.11-10.2.1.100)
- **Enterprise Zone: 10.2.2.0/24**
  - Router: 10.2.2.1/24
  - Web Server: 10.2.2.10/24
  - DNS Server: 10.2.2.11/24
  - DHCP Server: 10.2.2.12/24
  - PC: DHCP (range 10.2.2.21-10.2.2.100)
- **Public Zone: 10.2.3.0/24**
  - Router: 10.2.3.1/24
  - PC: DHCP (range 10.2.3.11-10.2.3.100)

#### KURONEXUS (10.3.x.x/24):
- **Border Router:**
  - Fa0/0 (ke eksternal): 192.168.100.3/24
  - Fa0/1 (ke Yamindralia): 192.168.200.1/24
  - Fa1/0 (ke internal switch): 10.3.0.1/24
- **Government Zone: 10.3.1.0/24**
  - Router: 10.3.1.1/24
  - PC: DHCP (range 10.3.1.11-10.3.1.100)
- **Enterprise Zone: 10.3.2.0/24**
  - Router: 10.3.2.1/24
  - Web Server: 10.3.2.10/24
  - DNS Server: 10.3.2.11/24
  - DHCP Server: 10.3.2.12/24
  - PC: DHCP (range 10.3.2.21-10.3.2.100)
- **Public Zone (VLAN):**
  - **VLAN 30 (Academy): 10.3.3.0/24**
    - Router: 10.3.3.1/24
    - PCs: DHCP (range 10.3.3.11-10.3.3.100)
  - **VLAN 40 (Business): 10.3.4.0/24**
    - Router: 10.3.4.1/24
    - PCs: DHCP (range 10.3.4.11-10.3.4.100)
  - **VLAN 50 (Communal): 10.3.5.0/24**
    - Router: 10.3.5.1/24
    - Wireless Devices: DHCP (range 10.3.5.11-10.3.5.100)

#### YAMINDRALIA (10.4.x.x/24):
- **Border Router:**
  - Fa0/0 (ke Kuronexus): 192.168.200.2/24
  - Fa0/1 (ke internal switch): 10.4.0.1/24
- **Government Zone: 10.4.1.0/24**
  - Router: 10.4.1.1/24
  - PC: DHCP (range 10.4.1.11-10.4.1.100)
- **Enterprise Zone: 10.4.2.0/24**
  - Router: 10.4.2.1/24
  - Web Server: 10.4.2.10/24
  - DNS Server: 10.4.2.11/24
  - DHCP Server: 10.4.2.12/24
  - PC: DHCP (range 10.4.2.21-10.4.2.100)
- **Public Zone: 10.4.3.0/24**
  - Router: 10.4.3.1/24
  - PC: DHCP (range 10.4.3.11-10.4.3.100)

## 2. Internal Routing - OSPF Multi-Area (2 poin)

### Konfigurasi OSPF pada setiap router:

#### Gokouloryn Border Router:
```cisco
enable
configure terminal
hostname G_BorderRouter
router ospf 1
router-id 1.1.1.1
network 10.1.0.0 0.0.0.255 area 0
passive-interface fa0/0
exit
```

#### Gokouloryn Government Router:
```cisco
enable
configure terminal
hostname G_GovRouter
interface fa0/0
ip address 10.1.0.2 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.1.1.1 255.255.255.0
no shutdown
exit
router ospf 1
router-id 1.1.1.2
network 10.1.0.0 0.0.0.255 area 0
network 10.1.1.0 0.0.0.255 area 1
exit
```

#### Gokouloryn Enterprise Router:
```cisco
enable
configure terminal
hostname G_EntRouter
interface fa0/0
ip address 10.1.0.3 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.1.2.1 255.255.255.0
no shutdown
exit
router ospf 1
router-id 1.1.1.3
network 10.1.0.0 0.0.0.255 area 0
network 10.1.2.0 0.0.0.255 area 2
exit
```

#### Gokouloryn Public Router:
```cisco
enable
configure terminal
hostname G_PubRouter
interface fa0/0
ip address 10.1.0.4 255.255.255.0
no shutdown
exit
interface fa0/1
ip address 10.1.3.1 255.255.255.0
no shutdown
exit
router ospf 1
router-id 1.1.1.4
network 10.1.0.0 0.0.0.255 area 0
network 10.1.3.0 0.0.0.255 area 3
exit
```

*Ulangi pola yang sama untuk Rurinthia (router-id 2.x.x.x), Kuronexus (router-id 3.x.x.x), dan Yamindralia (router-id 4.x.x.x)*

## 3. DHCP Configuration (1.5 poin)

### Konfigurasi DHCP Server di setiap negara:

#### Gokouloryn DHCP Server (10.1.2.12):
```cisco
enable
configure terminal
ip dhcp pool GOV_POOL
network 10.1.1.0 255.255.255.0
default-router 10.1.1.1
dns-server 10.1.2.11
exit
ip dhcp pool ENT_POOL
network 10.1.2.0 255.255.255.0
default-router 10.1.2.1
dns-server 10.1.2.11
exit
ip dhcp pool PUB_POOL
network 10.1.3.0 255.255.255.0
default-router 10.1.3.1
dns-server 10.1.2.11
exit
ip dhcp excluded-address 10.1.1.1 10.1.1.10
ip dhcp excluded-address 10.1.2.1 10.1.2.20
ip dhcp excluded-address 10.1.3.1 10.1.3.10
```

### Konfigurasi DHCP Helper pada router:

#### Government Router:
```cisco
interface fa0/1
ip helper-address 10.1.2.12
exit
```

#### Public Router:
```cisco
interface fa0/1
ip helper-address 10.1.2.12
exit
```

*Ulangi untuk semua negara dengan IP yang sesuai*

## 4. External Routing - BGP (2 poin)

### Konfigurasi BGP pada Border Router:

#### Gokouloryn Border Router:
```cisco
router bgp 65001
bgp router-id 1.1.1.1
neighbor 192.168.100.2 remote-as 65002
neighbor 192.168.100.3 remote-as 65003
network 10.1.0.0 mask 255.255.0.0
exit
```

#### Rurinthia Border Router:
```cisco
router bgp 65002
bgp router-id 2.2.2.2
neighbor 192.168.100.1 remote-as 65001
neighbor 192.168.100.3 remote-as 65003
network 10.2.0.0 mask 255.255.0.0
exit
```

#### Kuronexus Border Router:
```cisco
router bgp 65003
bgp router-id 3.3.3.3
neighbor 192.168.100.1 remote-as 65001
neighbor 192.168.100.2 remote-as 65002
neighbor 192.168.200.2 remote-as 65004
network 10.3.0.0 mask 255.255.0.0
exit
```

#### Yamindralia Border Router:
```cisco
router bgp 65004
bgp router-id 4.4.4.4
neighbor 192.168.200.1 remote-as 65003
network 10.4.0.0 mask 255.255.0.0
exit
```

### Redistribusi OSPF ke BGP:
```cisco
router bgp 65001
redistribute ospf 1
exit
router ospf 1
redistribute bgp 65001 subnets
exit
```

## 5. VLAN Configuration - Kuronexus Public Zone (1.25 poin)

### Konfigurasi Switch untuk VLAN:
```cisco
enable
configure terminal
vlan 30
name Academy
exit
vlan 40
name Business
exit
vlan 50
name Communal
exit

interface range fa0/1-5
switchport mode access
switchport access vlan 30
exit

interface range fa0/6-10
switchport mode access
switchport access vlan 40
exit

interface range fa0/11-15
switchport mode access
switchport access vlan 50
exit

interface fa0/24
switchport mode trunk
switchport trunk allowed vlan 30,40,50
exit
```

### Konfigurasi Router-on-a-Stick:
```cisco
enable
configure terminal
interface fa0/1
no shutdown
exit
interface fa0/1.30
encapsulation dot1Q 30
ip address 10.3.3.1 255.255.255.0
exit
interface fa0/1.40
encapsulation dot1Q 40
ip address 10.3.4.1 255.255.255.0
exit
interface fa0/1.50
encapsulation dot1Q 50
ip address 10.3.5.1 255.255.255.0
exit

router ospf 1
network 10.3.3.0 0.0.0.255 area 0
network 10.3.4.0 0.0.0.255 area 0
network 10.3.5.0 0.0.0.255 area 0
exit
```

### DHCP Pool untuk VLAN:
```cisco
ip dhcp pool VLAN30_POOL
network 10.3.3.0 255.255.255.0
default-router 10.3.3.1
dns-server 10.3.2.11
exit
ip dhcp pool VLAN40_POOL
network 10.3.4.0 255.255.255.0
default-router 10.3.4.1
dns-server 10.3.2.11
exit
ip dhcp pool VLAN50_POOL
network 10.3.5.0 255.255.255.0
default-router 10.3.5.1
dns-server 10.3.2.11
exit
```

## 6. Wireless Network - VLAN 50 Communal (1 poin)

### Tambahkan perangkat:
- 1 Wireless Access Point
- 1 Smartphone
- 1 PC dengan Wireless NIC

### Konfigurasi Access Point:
- SSID: "Acces Point"
- Security: -
- Password: -
- VLAN: 50









