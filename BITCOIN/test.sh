#!/bin/bash
# Skrip untuk menguji jaringan Blockchain sederhana

# Definisikan alamat node untuk kemudahan
NODE_1="http://localhost:5000"
NODE_2="http://localhost:5001"
NODE_3="http://localhost:5002"

echo "====================================================="
echo "🔬 1. MEMERIKSA STATUS AWAL JARINGAN"
echo "====================================================="
# Memeriksa rantai awal (hanya berisi Genesis Block)
curl -s $NODE_1/chain | jq '.chain | length'
echo "Peers di Node 1:"
curl -s $NODE_1/peers | jq .

echo "\n====================================================="
echo "🌱 2. INISIALISASI SALDO AWAL (PRE-MINE)"
echo "====================================================="
echo "--> Memberikan 100 koin ke Alice dari 'Network'"
curl -X POST -H "Content-Type: application/json" -d '{"sender": "Network", "recipient": "Alice", "amount": 100}' $NODE_1/transaction

echo "\n--> Memberikan 100 koin ke Charlie dari 'Network'"
curl -X POST -H "Content-Type: application/json" -d '{"sender": "Network", "recipient": "Charlie", "amount": 100}' $NODE_1/transaction

sleep 1

echo "\n--> Menambang Blok #1 untuk mengonfirmasi saldo awal (miner: GenesisMiner)"
curl -s "$NODE_1/mine?miner_address=GenesisMiner" | jq .

# Beri waktu agar blok pertama tersebar ke seluruh jaringanJ
sleep 2

echo "\n--> Verifikasi Saldo Awal Alice:"
curl -s $NODE_2/balance/Alice | jq .

echo "\n====================================================="
echo "💸 3. MEMBUAT TRANSAKSI PENGGUNA"
echo "====================================================="
# Sekarang Alice memiliki saldo dan bisa mengirim koin
echo "--> Mengirim transaksi: Alice -> Bob (10.5)"
curl -X POST -H "Content-Type: application/json" -d '{"sender": "Alice", "recipient": "Bob", "amount": 10.5}' $NODE_1/transaction

echo "\n--> Mengirim transaksi: Charlie -> Diana (3.2)"
curl -X POST -H "Content-Type: application/json" -d '{"sender": "Charlie", "recipient": "Diana", "amount": 3.2}' $NODE_2/transaction

sleep 2

echo "\n====================================================="
echo "⛏️  4. MINING BLOK KEDUA"
echo "====================================================="
echo "--> Node 3 akan me-mining blok baru (miner: Adha)"
curl -s "$NODE_3/mine?miner_address=Adha" | jq .

sleep 2

echo "\n====================================================="
echo "⛓️  5. VERIFIKASI SINKRONISASI RANTAI"
echo "====================================================="
echo "--> Memeriksa rantai di Node 1 (seharusnya sudah tersinkronisasi)"
curl -s $NODE_1/chain | jq '.chain | length'

echo "\n--> Memeriksa rantai di Node 2 (seharusnya sudah tersinkronisasi)"
curl -s $NODE_2/chain | jq '.chain | length'

echo "\n====================================================="
echo "💰 6. MEMERIKSA SALDO AKUN AKHIR"
echo "====================================================="
echo "--> Saldo Alice (seharusnya berkurang):"
curl -s $NODE_1/balance/Alice | jq
echo "\n--> Saldo Bob (seharusnya bertambah):"
curl -s $NODE_1/balance/Bob | jq
echo "\n--> Saldo miner (Adha):"
curl -s $NODE_1/balance/Adha | jq

echo "\n====================================================="
echo "🎉 PENGUJIAN DASAR SELESAI"
echo "====================================================="