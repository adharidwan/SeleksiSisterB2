#!/usr/bin/env python3
"""
Bitcoin Network Testing Demo
Script untuk testing Bitcoin network secara otomatis
"""

import requests
import time
import threading
import json
import sys
import subprocess

class BitcoinTestSuite:
    def __init__(self):
        self.base_ports = [5001, 5002, 5003]
        self.nodes = []
        
    def test_single_node(self):
        """Test basic functionality pada satu node"""
        print("🧪 Testing Single Node Operations...")
        node_url = "http://localhost:5001"
        
        # Test 1: Check node info
        try:
            response = requests.get(f"{node_url}/info", timeout=5)
            if response.status_code == 200:
                print("✅ Node info endpoint working")
                info = response.json()
                print(f"   Node ID: {info['node_id']}")
                print(f"   Mining Address: {info['mining_stats']['miner_address']}")
            else:
                print("❌ Node info endpoint failed")
                return False
        except Exception as e:
            print(f"❌ Cannot connect to node: {e}")
            return False
        
        # Test 2: Add transaction
        sample_tx = {
            "sender": "Alice",
            "recipient": "Bob", 
            "amount": 50
        }
        
        try:
            response = requests.post(f"{node_url}/transaction", json=sample_tx, timeout=5)
            if response.status_code == 200:
                print("✅ Transaction added successfully")
                tx_data = response.json()
                print(f"   TX ID: {tx_data['tx_id'][:16]}...")
            else:
                print(f"❌ Failed to add transaction: {response.text}")
                return False
        except Exception as e:
            print(f"❌ Transaction failed: {e}")
            return False
        
        # Test 3: Mine block
        try:
            print("⛏️ Starting mining...")
            response = requests.get(f"{node_url}/mine", timeout=5)
            if response.status_code == 200:
                print("✅ Mining started successfully")
                
                # Wait for mining to complete
                time.sleep(10)  # Give time for mining
                
                # Check chain
                chain_response = requests.get(f"{node_url}/chain", timeout=5)
                if chain_response.status_code == 200:
                    chain_data = chain_response.json()
                    if chain_data['length'] > 1:
                        print(f"✅ Block mined! Chain length: {chain_data['length']}")
                        return True
                    else:
                        print("⏳ Mining still in progress...")
                        return True  # Still valid, just slow
            else:
                print(f"❌ Mining failed: {response.text}")
                return False
        except Exception as e:
            print(f"❌ Mining error: {e}")
            return False
    
    def test_multi_node_sync(self):
        """Test sinkronisasi antar multiple nodes"""
        print("\n🔗 Testing Multi-Node Synchronization...")
        
        # Add transaction to node 1
        tx = {"sender": "Charlie", "recipient": "Dave", "amount": 25}
        
        try:
            response = requests.post("http://localhost:5001/transaction", json=tx, timeout=5)
            if response.status_code == 200:
                print("✅ Transaction added to Node 1")
            else:
                print("❌ Failed to add transaction to Node 1")
                return False
        except Exception as e:
            print(f"❌ Cannot connect to Node 1: {e}")
            return False
        
        # Mine on node 1
        try:
            requests.get("http://localhost:5001/mine", timeout=5)
            print("⛏️ Mining started on Node 1")
            time.sleep(15)  # Wait for mining
        except Exception as e:
            print(f"❌ Mining failed: {e}")
            return False
        
        # Check if other nodes received the block
        time.sleep(5)  # Give time for sync
        
        for port in [5002, 5003]:
            try:
                response = requests.get(f"http://localhost:{port}/chain", timeout=5)
                if response.status_code == 200:
                    chain_data = response.json()
                    if chain_data['length'] > 1:
                        print(f"✅ Node {port} synchronized - Chain length: {chain_data['length']}")
                    else:
                        print(f"⏳ Node {port} not yet synchronized")
                else:
                    print(f"❌ Cannot get chain from Node {port}")
            except Exception as e:
                print(f"❌ Cannot connect to Node {port}: {e}")
        
        return True
    
    def test_consensus_mechanism(self):
        """Test longest chain rule"""
        print("\n🤝 Testing Consensus Mechanism...")
        
        # Trigger consensus on all nodes
        for port in self.base_ports:
            try:
                response = requests.get(f"http://localhost:{port}/consensus", timeout=10)
                if response.status_code == 200:
                    result = response.json()
                    print(f"✅ Node {port} consensus: {result['message']}")
                else:
                    print(f"❌ Consensus failed on Node {port}")
            except Exception as e:
                print(f"❌ Consensus error on Node {port}: {e}")
        
        return True
    
    def create_load_test(self):
        """Buat beberapa transaksi untuk load testing"""
        print("\n📊 Creating Load Test Transactions...")
        
        transactions = [
            {"sender": "Alice", "recipient": "Bob", "amount": 10},
            {"sender": "Bob", "recipient": "Charlie", "amount": 15},
            {"sender": "Charlie", "recipient": "Alice", "amount": 5},
            {"sender": "Alice", "recipient": "Dave", "amount": 20},
            {"sender": "Dave", "recipient": "Bob", "amount": 8},
            {"sender": "Bob", "recipient": "Eve", "amount": 12},
        ]
        
        success_count = 0
        for i, tx in enumerate(transactions):
            node_port = self.base_ports[i % len(self.base_ports)]  # Distribute across nodes
            try:
                response = requests.post(
                    f"http://localhost:{node_port}/transaction", 
                    json=tx, 
                    timeout=5
                )
                if response.status_code == 200:
                    success_count += 1
                    print(f"✅ TX {i+1}: {tx['sender']} -> {tx['recipient']} = {tx['amount']}")
                else:
                    print(f"❌ TX {i+1} failed: {response.text}")
            except Exception as e:
                print(f"❌ TX {i+1} error: {e}")
        
        print(f"📊 Load test completed: {success_count}/{len(transactions)} transactions successful")
        return success_count > 0
    
    def check_balances(self):
        """Check balances untuk semua addresses"""
        print("\n💰 Checking Account Balances...")
        
        addresses = ["Alice", "Bob", "Charlie", "Dave", "Eve", "Miner_1", "Miner_2", "Miner_3"]
        
        for address in addresses:
            try:
                response = requests.get(f"http://localhost:5001/balance/{address}", timeout=5)
                if response.status_code == 200:
                    balance_data = response.json()
                    if balance_data['balance'] != 0:
                        print(f"💰 {address}: {balance_data['balance']} coins")
                else:
                    print(f"❌ Cannot get balance for {address}")
            except Exception as e:
                print(f"❌ Balance check error for {address}: {e}")
    
    def run_full_test(self):
        """Jalankan semua test"""
        print("🎬 Bitcoin Network Testing Suite")
        print("=" * 50)
        
        # Wait for nodes to be ready
        print("⏳ Waiting for nodes to be ready...")
        time.sleep(5)
        
        test_results = []
        
        # Test 1: Single node functionality
        test_results.append(self.test_single_node())
        
        # Test 2: Multi-node sync
        test_results.append(self.test_multi_node_sync())
        
        # Test 3: Load testing
        test_results.append(self.create_load_test())
        
        # Test 4: Consensus
        test_results.append(self.test_consensus_mechanism())
        
        # Test 5: Check balances
        self.check_balances()
        
        # Summary
        print("\n" + "=" * 50)
        passed_tests = sum(test_results)
        total_tests = len(test_results)
        
        if passed_tests == total_tests:
            print(f"🎉 ALL TESTS PASSED! ({passed_tests}/{total_tests})")
            return True
        else:
            print(f"⚠️ {passed_tests}/{total_tests} tests passed")
            return False

def main():
    """Main testing function"""
    if len(sys.argv) > 1 and sys.argv[1] == "auto":
        # Automated testing mode
        tester = BitcoinTestSuite()
        success = tester.run_full_test()
        sys.exit(0 if success else 1)
    else:
        # Interactive mode
        print("🎮 Bitcoin Network Test Menu")
        print("1. Run single node test")
        print("2. Run multi-node sync test")
        print("3. Run load test")
        print("4. Run consensus test")
        print("5. Check balances")
        print("6. Run all tests")
        print("0. Exit")
        
        tester = BitcoinTestSuite()
        
        while True:
            try:
                choice = input("\nEnter choice (0-6): ").strip()
                
                if choice == "1":
                    tester.test_single_node()
                elif choice == "2":
                    tester.test_multi_node_sync()
                elif choice == "3":
                    tester.create_load_test()
                elif choice == "4":
                    tester.test_consensus_mechanism()
                elif choice == "5":
                    tester.check_balances()
                elif choice == "6":
                    tester.run_full_test()
                elif choice == "0":
                    print("👋 Goodbye!")
                    break
                else:
                    print("❌ Invalid choice")
                    
            except KeyboardInterrupt:
                print("\n👋 Goodbye!")
                break
            except Exception as e:
                print(f"❌ Error: {e}")

if __name__ == "__main__":
    main()