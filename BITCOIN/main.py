#!/usr/bin/env python3
"""
Bitcoin Network Simulation - Main Entry Point
Implementasi Bitcoin Network sederhana untuk Tugas Sister Lab
"""

import sys
import time
import argparse
import threading
from blockchain import Blockchain
from mining import Miner
from network import NetworkNode
from transaction import Transaction

def create_sample_transactions(node_url: str):
    """Buat sample transactions untuk testing"""
    import requests
    
    sample_transactions = [
        {"sender": "Alice", "recipient": "Bob", "amount": 50},
        {"sender": "Bob", "recipient": "Charlie", "amount": 25},
        {"sender": "Charlie", "recipient": "Alice", "amount": 10},
        {"sender": "Alice", "recipient": "Dave", "amount": 30},
        {"sender": "Dave", "recipient": "Bob", "amount": 15},
    ]
    
    print(f"🧪 Creating sample transactions for {node_url}...")
    for tx in sample_transactions:
        try:
            response = requests.post(f"{node_url}/transaction", json=tx, timeout=5)
            if response.status_code == 200:
                print(f"✅ Transaction: {tx['sender']} -> {tx['recipient']} = {tx['amount']}")
            else:
                print(f"❌ Failed to create transaction: {response.json()}")
        except Exception as e:
            print(f"❌ Error creating transaction: {e}")

def run_node(node_id: int, port: int, peers: list = None, miner_name: str = None, auto_mine: bool = False):
    """Jalankan satu node Bitcoin"""
    
    # Setup
    miner_address = miner_name or f"Miner_{node_id}"
    
    print(f"🚀 Initializing Bitcoin Node {node_id}...")
    print(f"⛏️ Miner: {miner_address}")
    
    # Inisialisasi blockchain dengan difficulty yang reasonable untuk demo
    blockchain = Blockchain(difficulty=3)  # 3 leading zeros untuk demo cepat
    
    # Inisialisasi miner
    miner = Miner(blockchain, miner_address)
    
    # Inisialisasi network node
    node = NetworkNode('localhost', port, blockchain, miner, f"node_{node_id}")
    
    # Tambahkan peers jika ada
    if peers:
        for peer in peers:
            node.add_peer(peer)
            print(f"🤝 Added peer: {peer}")
    
    # Auto mining jika diminta
    if auto_mine:
        def auto_mining():
            time.sleep(10)  # Tunggu node siap
            while True:
                if len(blockchain.transaction_pool) >= 2:  # Mine ketika ada minimal 2 transaksi
                    print(f"🤖 Auto mining triggered for Node {node_id}")
                    miner.mine_block()
                time.sleep(15)  # Check setiap 15 detik
        
        threading.Thread(target=auto_mining, daemon=True).start()
        print("🤖 Auto mining enabled")
    
    # Jalankan node
    print(f"🌐 Node {node_id} starting on http://localhost:{port}")
    print("=" * 50)
    
    try:
        node.run(debug=False, auto_sync=True)
    except KeyboardInterrupt:
        print(f"\n🛑 Node {node_id} shutting down...")
        miner.stop_mining()

def main():
    """Main function dengan argument parsing"""
    parser = argparse.ArgumentParser(description='Bitcoin Network Simulation')
    parser.add_argument('node_id', type=int, choices=[1, 2, 3, 4, 5], 
                       help='Node ID (1-5)')
    parser.add_argument('--miner-name', type=str, 
                       help='Custom miner name')
    parser.add_argument('--auto-mine', action='store_true',
                       help='Enable automatic mining')
    parser.add_argument('--sample-tx', action='store_true',
                       help='Create sample transactions after startup')
    parser.add_argument('--difficulty', type=int, default=3, choices=range(1, 7),
                       help='Mining difficulty (1-6)')
    
    args = parser.parse_args()
    
    # Konfigurasi nodes (bisa diperluas hingga 5 node)
    node_configs = {
        1: {
            'port': 5001,
            'peers': ['http://localhost:5002', 'http://localhost:5003']
        },
        2: {
            'port': 5002,
            'peers': ['http://localhost:5001', 'http://localhost:5003']
        },
        3: {
            'port': 5003,
            'peers': ['http://localhost:5001', 'http://localhost:5002']
        },
        4: {
            'port': 5004,
            'peers': ['http://localhost:5001', 'http://localhost:5002', 'http://localhost:5003']
        },
        5: {
            'port': 5005,
            'peers': ['http://localhost:5001', 'http://localhost:5002', 'http://localhost:5003']
        }
    }
    
    if args.node_id not in node_configs:
        print(f"❌ Invalid node ID. Use 1-{len(node_configs)}")
        sys.exit(1)
    
    config = node_configs[args.node_id]
    
    # Create sample transactions jika diminta
    if args.sample_tx:
        def create_sample_tx_delayed():
            time.sleep(5)  # Tunggu node siap
            create_sample_transactions(f"http://localhost:{config['port']}")
        
        threading.Thread(target=create_sample_tx_delayed, daemon=True).start()
    
    # Jalankan node
    run_node(
        args.node_id,
        config['port'],
        config['peers'],
        args.miner_name,
        args.auto_mine
    )

def quick_demo():
    """Demo cepat untuk testing"""
    print("🎬 Bitcoin Network Quick Demo")
    print("=" * 40)
    
    # Node configs untuk demo
    demo_configs = [
        {"id": 1, "port": 5001},
        {"id": 2, "port": 5002},
        {"id": 3, "port": 5003}
    ]
    
    print("Starting 3 nodes for demo...")
    
    # Instruksi untuk user
    print("\n📋 Demo Instructions:")
    print("1. Run in 3 different terminals:")
    for config in demo_configs:
        print(f"   Terminal {config['id']}: python main.py {config['id']} --auto-mine")
    
    print("\n2. Wait for nodes to connect")
    print("3. Add transactions via API:")
    print("   curl -X POST http://localhost:5001/transaction \\")
    print("        -H 'Content-Type: application/json' \\")
    print("        -d '{\"sender\":\"Alice\",\"recipient\":\"Bob\",\"amount\":50}'")
    
    print("\n4. Check blockchain:")
    print("   curl http://localhost:5001/chain")
    
    print("\n5. Get node info:")
    print("   curl http://localhost:5001/info")

if __name__ == '__main__':
    try:
        # Jika dipanggil tanpa argument, tampilkan demo instructions
        if len(sys.argv) == 1:
            quick_demo()
            sys.exit(0)
        
        # Parse arguments dan jalankan
        main()
        
    except KeyboardInterrupt:
        print("\n👋 Goodbye!")
        sys.exit(0)
    except Exception as e:
        print(f"❌ Fatal error: {e}")
        sys.exit(1)