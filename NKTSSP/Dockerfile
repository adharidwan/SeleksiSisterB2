# Menggunakan base image Ubuntu 22.04 yang ringan dan modern
FROM ubuntu:22.04

# Mencegah prompt interaktif selama instalasi paket
ENV DEBIAN_FRONTEND=noninteractive

# Update package list dan install dependensi yang dibutuhkan
# - build-essential: Termasuk GCC dan C library headers (untuk mengatasi error string.h)
# - make: Untuk menjalankan perintah dari Makefile
# - nasm: Assembler yang dibutuhkan untuk file .asm
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    make \
    nasm \
    && rm -rf /var/lib/apt/lists/*

# Set direktori kerja di dalam container
WORKDIR /app

# Salin semua file yang dibutuhkan dari direktori lokal ke dalam container
# Ini termasuk source code, makefile, dan file HTML
COPY main.asm auth.c Makefile index.html secret.html ./

# Jalankan 'make' untuk mengompilasi dan me-link semua source code
# menjadi satu file executable bernama 'main' sesuai aturan di Makefile.
RUN make

# Expose port 8080, port default yang digunakan oleh server
EXPOSE 8080

# Perintah untuk menjalankan server saat container dimulai.
# Server dijalankan pada port 8080.
CMD ["./main", "8080"]