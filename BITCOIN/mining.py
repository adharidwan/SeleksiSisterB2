import time
import threading
from typing import List, Dict, Any, Optional, Callable
from blockchain import Block, Blockchain
from transaction import Transaction, create_mining_reward_transaction

class Miner:
    """Implementasi Bitcoin Miner dengan Proof of Work"""
    
    def __init__(self, blockchain: Blockchain, miner_address: str = "DefaultMiner"):
        self.blockchain = blockchain
        self.miner_address = miner_address
        self.is_mining = False
        self.mining_stats = {
            "blocks_mined": 0,
            "total_mining_time": 0,
            "average_mining_time": 0,
            "hash_rate": 0
        }
        self.stop_mining_flag = False
        self.on_block_mined_callback: Optional[Callable] = None
    
    def set_callback(self, callback: Callable):
        """Set callback yang dipanggil ketika berhasil mining block"""
        self.on_block_mined_callback = callback
    
    def mine_block(self, max_transactions: int = 10) -> Optional[Block]:
        """Mining block baru dengan proof of work"""
        if self.is_mining:
            print("Already mining a block")
            return None
        
        try:
            self.is_mining = True
            self.stop_mining_flag = False
            
            # Ambil transaksi dari pool
            pending_transactions = self.blockchain.transaction_pool.copy()
            
            # Filter transaksi valid berdasarkan balance
            valid_transactions = []
            for tx_data in pending_transactions[:max_transactions]:
                if self.blockchain.validate_transaction(tx_data):
                    valid_transactions.append(tx_data)
            
            # Tambahkan mining reward transaction
            mining_reward_tx = create_mining_reward_transaction(
                self.miner_address, 
                self.blockchain.mining_reward
            )
            valid_transactions.append(mining_reward_tx.to_dict())
            
            # Buat block baru
            previous_block = self.blockchain.get_latest_block()
            new_block = Block(
                index=previous_block.index + 1,
                timestamp=time.time(),
                data=valid_transactions,
                previous_hash=previous_block.hash
            )
            
            # Proof of Work
            print(f"🔨 Mining block {new_block.index} with {len(valid_transactions)} transactions...")
            print(f"📊 Target: {self.blockchain.difficulty} leading zeros")
            
            start_time = time.time()
            hash_count = 0
            
            while not new_block.hash.startswith('0' * self.blockchain.difficulty):
                if self.stop_mining_flag:
                    print("⏹️ Mining stopped by user")
                    return None
                
                new_block.nonce += 1
                new_block.hash = new_block.calculate_hash()
                hash_count += 1
                
                # Print progress setiap 100,000 hash
                if hash_count % 100000 == 0:
                    elapsed = time.time() - start_time
                    hash_rate = hash_count / elapsed if elapsed > 0 else 0
                    print(f"⚡ Hashes: {hash_count:,} | Rate: {hash_rate:,.0f} H/s | Nonce: {new_block.nonce}")
            
            end_time = time.time()
            mining_time = end_time - start_time
            hash_rate = hash_count / mining_time if mining_time > 0 else 0
            
            # Update mining stats
            self.mining_stats["blocks_mined"] += 1
            self.mining_stats["total_mining_time"] += mining_time
            self.mining_stats["average_mining_time"] = (
                self.mining_stats["total_mining_time"] / self.mining_stats["blocks_mined"]
            )
            self.mining_stats["hash_rate"] = hash_rate
            
            # Tampilkan hasil mining
            print(f"✅ Block {new_block.index} mined successfully!")
            print(f"🔑 Hash: {new_block.hash}")
            print(f"🔢 Nonce: {new_block.nonce:,}")
            print(f"⏱️ Mining time: {mining_time:.2f} seconds")
            print(f"⚡ Hash rate: {hash_rate:,.0f} H/s")
            print(f"💰 Reward: {self.blockchain.mining_reward} coins")
            
            # Tambahkan ke blockchain
            self.blockchain.chain.append(new_block)
            
            # Hapus transaksi yang sudah diproses dari pool
            processed_tx_ids = [tx['tx_id'] for tx in valid_transactions]
            self.blockchain.transaction_pool = [
                tx for tx in self.blockchain.transaction_pool
                if tx['tx_id'] not in processed_tx_ids
            ]
            
            # Panggil callback jika ada
            if self.on_block_mined_callback:
                self.on_block_mined_callback(new_block)
            
            return new_block
            
        except Exception as e:
            print(f"❌ Error during mining: {e}")
            return None
        
        finally:
            self.is_mining = False
    
    def mine_continuously(self, interval: float = 10.0):
        """Mining secara continuous dengan interval tertentu"""
        def mining_loop():
            while not self.stop_mining_flag:
                if len(self.blockchain.transaction_pool) > 0:
                    block = self.mine_block()
                    if block:
                        print(f"🎉 Continuous mining: Block {block.index} added to chain")
                
                time.sleep(interval)
        
        mining_thread = threading.Thread(target=mining_loop, daemon=True)
        mining_thread.start()
        return mining_thread
    
    def stop_mining(self):
        """Hentikan proses mining"""
        self.stop_mining_flag = True
        self.is_mining = False
        print("🛑 Mining stop requested")
    
    def get_mining_stats(self) -> Dict[str, Any]:
        """Dapatkan statistik mining"""
        return {
            "miner_address": self.miner_address,
            "is_mining": self.is_mining,
            "blocks_mined": self.mining_stats["blocks_mined"],
            "total_mining_time": round(self.mining_stats["total_mining_time"], 2),
            "average_mining_time": round(self.mining_stats["average_mining_time"], 2),
            "current_hash_rate": round(self.mining_stats["hash_rate"], 0),
            "difficulty": self.blockchain.difficulty,
            "pending_transactions": len(self.blockchain.transaction_pool),
            "total_balance": self.blockchain.get_balance(self.miner_address)
        }
    
    def estimate_mining_time(self) -> float:
        """Estimasi waktu mining berdasarkan difficulty dan hash rate"""
        if self.mining_stats["hash_rate"] == 0:
            return float('inf')
        
        # Estimasi berdasarkan difficulty dan hash rate rata-rata
        expected_hashes = 16 ** self.blockchain.difficulty  # Roughly
        estimated_time = expected_hashes / self.mining_stats["hash_rate"]
        return estimated_time

class MiningPool:
    """Pool untuk multiple miners (bonus feature)"""
    
    def __init__(self, blockchain: Blockchain):
        self.blockchain = blockchain
        self.miners = {}
        self.pool_stats = {
            "total_blocks_mined": 0,
            "total_hashrate": 0,
            "active_miners": 0
        }
    
    def add_miner(self, miner_id: str, miner: Miner):
        """Tambahkan miner ke pool"""
        self.miners[miner_id] = miner
        miner.set_callback(self.on_block_found)
    
    def remove_miner(self, miner_id: str):
        """Hapus miner dari pool"""
        if miner_id in self.miners:
            self.miners[miner_id].stop_mining()
            del self.miners[miner_id]
    
    def on_block_found(self, block: Block):
        """Callback ketika ada miner yang menemukan block"""
        self.pool_stats["total_blocks_mined"] += 1
        
        # Stop all other miners
        for miner in self.miners.values():
            if miner.is_mining:
                miner.stop_mining()
        
        print(f"🏆 Pool found block {block.index}!")
    
    def start_pool_mining(self):
        """Mulai mining untuk semua miners dalam pool"""
        print(f"🏊 Starting pool mining with {len(self.miners)} miners")
        
        for miner_id, miner in self.miners.items():
            mining_thread = threading.Thread(
                target=miner.mine_continuously,
                args=(5.0,),  # 5 second interval
                daemon=True
            )
            mining_thread.start()
            print(f"⛏️ Miner {miner_id} started")
    
    def get_pool_stats(self) -> Dict[str, Any]:
        """Statistik mining pool"""
        active_miners = sum(1 for m in self.miners.values() if m.is_mining)
        total_hashrate = sum(m.mining_stats["hash_rate"] for m in self.miners.values())
        
        return {
            "total_miners": len(self.miners),
            "active_miners": active_miners,
            "total_hashrate": total_hashrate,
            "blocks_mined": self.pool_stats["total_blocks_mined"],
            "miners": {
                miner_id: miner.get_mining_stats() 
                for miner_id, miner in self.miners.items()
            }
        }

# Utility functions
def calculate_mining_difficulty(blockchain: Blockchain, target_time: float = 10.0) -> int:
    """Hitung difficulty berdasarkan waktu mining yang diinginkan"""
    if len(blockchain.chain) < 2:
        return blockchain.difficulty
    
    # Hitung rata-rata waktu mining dari 5 block terakhir
    recent_blocks = blockchain.chain[-5:]
    if len(recent_blocks) < 2:
        return blockchain.difficulty
    
    total_time = recent_blocks[-1].timestamp - recent_blocks[0].timestamp
    avg_time = total_time / (len(recent_blocks) - 1)
    
    if avg_time < target_time * 0.8:  # Terlalu cepat, naikkan difficulty
        return min(blockchain.difficulty + 1, 8)  # Max difficulty 8
    elif avg_time > target_time * 1.2:  # Terlalu lambat, turunkan difficulty
        return max(blockchain.difficulty - 1, 1)  # Min difficulty 1
    
    return blockchain.difficulty

def validate_proof_of_work(block: Block, difficulty: int) -> bool:
    """Validasi proof of work untuk block"""
    return block.hash.startswith('0' * difficulty) and block.hash == block.calculate_hash()