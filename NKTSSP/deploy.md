# Panduan Deployment: NKTSSP di Azure VM dengan Docker & HTTPS

Dokumen ini merangkum seluruh proses deployment aplikasi NKTSSP, mulai dari persiapan sumber daya di Azure hingga mengamankan domain dengan sertifikat SSL (HTTPS) menggunakan Nginx sebagai reverse proxy.

## Prasyarat

Sebelum memulai, pastikan Anda memiliki:
1.  **Akun Azure** yang aktif.
2.  **Domain yang sudah terdaftar** dari registrar mana pun.
3.  Di komputer **lokal** Anda, terinstal:
    * **Azure CLI**: [Panduan Instalasi](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli)
    * **Docker Desktop**: [Panduan Instalasi](https://docs.docker.com/get-docker/)

---

### Langkah 1: Persiapan Sumber Daya di Azure

Langkah ini membuat semua infrastruktur dasar yang kita butuhkan di Azure.

#### 1.1 Definisikan Variabel
Buka terminal di komputer lokal Anda dan jalankan perintah ini untuk mempermudah proses. Ganti nilainya sesuai kebutuhan.

```bash
# Ganti dengan nama unik dan lokasi yang Anda inginkan
RESOURCE_GROUP="rg-nktsp-server"
LOCATION="southeastasia"
ACR_NAME="acrnktspserver$(date +%s)" # Membuat nama unik untuk ACR
VM_NAME="vm-nktsp-server"
IMAGE_NAME="nktsp-server:latest"
```

# Buat Resource Group
az group create --name $RESOURCE_GROUP --location "$LOCATION"

# Buat Azure Container Registry (ACR) untuk menyimpan image Docker
az acr create --resource-group $RESOURCE_GROUP --name $ACR_NAME --sku Basic

# Buat Virtual Machine (VM) Ubuntu
az vm create \
  --resource-group $RESOURCE_GROUP \
  --name $VM_NAME \
  --image Ubuntu2204 \
  --admin-username azureuser \
  --generate-ssh-keys

# Login ke ACR yang baru dibuat
az acr login --name $ACR_NAME

# Build image Docker
docker build -t ${ACR_NAME}.azurecr.io/$IMAGE_NAME .

# Push image ke ACR
docker push ${ACR_NAME}.azurecr.io/$IMAGE_NAME

# Konfigurasikan DNS Domain
1. Dapatkan IP Publik VM
2. Masuk ke Dashboard Registrar Domain Anda: Buka website tempat Anda membeli domain.
3. Buka Manajemen DNS: Buat A Record baru dengan konfigurasi berikut:
    Host/Name: @ atau biarkan kosong (untuk domain utama).
    Type: A
    Value/Points to: Alamat IP Publik VM Anda yang didapatkan dari langkah 1.
4. Tunggu Propagansi

# Konfigurasi VM 
1. SSH masuk ke VM Kemudian jalankan:
```
# Instal Docker
sudo apt-get update
sudo apt-get install -y docker.io
sudo usermod -aG docker $USER

# PENTING: Keluar dari SSH lalu masuk lagi agar izin Docker aktif
exit
```

2. Masuk lagi dan lanjutkan:
```
# Lanjutkan instalasi Nginx dan Certbot
sudo apt-get update
sudo apt-get install -y nginx certbot python3-certbot-nginx
```

3. Buat konfigurasi nginx
```
# Buat file konfigurasi
sudo nano /etc/nginx/sites-available/domainanda.com

# Isi dengan konfigurasi berikut (ganti server_name)
server {
    listen 80;
    server_name domainanda.com [www.domainanda.com](https://www.domainanda.com);

    location / {
        proxy_pass [http://127.0.0.1:8080](http://127.0.0.1:8080); # Arahkan ke Docker di localhost
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}

# Aktifkan dan restart Nginx
sudo ln -s /etc/nginx/sites-available/domainanda.com /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl restart nginx
```

4. Kembali ke azure-cli anda dan jalankan untuk membuka port
```
# Buka Port 80 (untuk verifikasi HTTP) dan 443 (untuk HTTPS)
az vm open-port --port 80 --resource-group $RESOURCE_GROUP --name $VM_NAME --priority 901
az vm open-port --port 443 --resource-group $RESOURCE_GROUP --name $VM_NAME --priority 902
```

5. Kembali ke VM
```
# Login ke Azure & ACR dari dalam VM
az login
az acr login --name $ACR_NAME

# Tarik image dari ACR
docker pull ${ACR_NAME}.azurecr.io/$IMAGE_NAME

# Jalankan container
docker run -d -p 127.0.0.1:8080:8080 --restart always --name nktsp-server ${ACR_NAME}.azurecr.io/$IMAGE_NAME
```

6. Terakhir hidupkan sertifikat SSL
```
# Ganti dengan domain Anda
sudo certbot --nginx -d domainanda.com -d [www.domainanda.com](https://www.domainanda.com)
```

# Hasil Deploy
hutaogaot.space