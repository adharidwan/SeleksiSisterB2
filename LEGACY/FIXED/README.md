# COBOL + FastAPI Debug Task

Your task:
1. Fix `main.cob` so it works correctly.
2. Fix `Dockerfile` so the app runs end-to-end.

Rules:
- **Do NOT modify** `app.py` or `index.html`.
- You can change anything in `main.cob` and `Dockerfile`.
- Input/output files (`input.txt`, `output.txt`, `accounts.txt`) are provided.

How to test:
```bash
docker build -t cobol-app .
docker run --rm -p 8000:8000 cobol-app
```

Feature:
The program default are running interest with 20% / 23 s, to disable it modify the following line at Dockerfile:
```
ARG APPLY_INTEREST_ARG=true
```
Into:
```
ARG APPLY_INTEREST_ARG=false
```

Bonus Feature

### 1. ✅ Indonesia Rupiah Conversion (2 points)
**Cara Pengerjaan**: 
- Menambahkan variabel `RAI-TO-IDR-RATE` dengan nilai 119714660 di `main.cob`
- Memodifikasi prosedur `DISPLAY-BALANCE-WITH-IDR` untuk menghitung konversi IDR
- Menggunakan `COMPUTE IDR-BALANCE = TMP-BALANCE * RAI-TO-IDR-RATE`
- Format output menampilkan kedua mata uang: `RAI STONE BALANCE: 1690.00 | IDR EQUIVALENT: Rp202,318,175,400`

### 2. ✅ Kubernetes Deployment (3 points)
**Cara Pengerjaan**:
- Membuat file `k8s-deploy.yaml` dengan konfigurasi lengkap:
  - Namespace untuk isolasi aplikasi
  - ConfigMap untuk menyimpan data accounts.txt
  - Deployment dengan init container untuk setup data
  - Service untuk exposing aplikasi
  - Ingress untuk routing traffic
- Membuat file `deploy.md` dengan panduan deployment step-by-step di Azure Kubernetes Service
- Konfigurasi health checks (liveness dan readiness probes)

### 3. ✅ Interest Calculation Feature (4 points)
**Cara Pengerjaan**:
- Menambahkan command line argument parsing di `main.cob`
- Implementasi prosedur `APPLY-INTEREST-IN-LOOP` yang berjalan infinite loop
- Prosedur `CALCULATE-ALL-INTEREST` untuk menghitung bunga 20% setiap 23 detik
- Menggunakan `CALL "C$SLEEP" USING SLEEP-DURATION-SEC` untuk delay
- File `start.sh` untuk menjalankan background process jika environment variable `APPLY_INTEREST=true`

### 4. ✅ Reverse Proxy (2 points)
**Cara Pengerjaan**:
- Konfigurasi Ingress NGINX Controller di Kubernetes manifest
- Menambahkan resource `Ingress` dengan `ingressClassName: nginx` di `k8s-deploy.yaml`
- Panduan instalasi Helm dan NGINX controller di `deploy.md`
- Setup routing HTTP traffic ke service aplikasi

### 5. ✅ Domain & HTTPS (2 points)
**Cara Pengerjaan**:
- Deploy aplikasi ke Azure Kubernetes Service dengan public IP
- Konfigurasi DNS record untuk domain `adha.caremo.id`

Deployment:
adha.caremo.id