import hashlib
import json
import time
from typing import List, Dict, Any, Optional

class Block:
    """Implementasi Bitcoin Block dengan struktur lengkap"""
    
    def __init__(self, index: int, timestamp: float, data: List[Dict], 
                 previous_hash: str, nonce: int = 0, merkle_root: str = None):
        self.index = index
        self.timestamp = timestamp
        self.data = data  # List of transactions
        self.previous_hash = previous_hash
        self.nonce = nonce
        self.merkle_root = merkle_root or self.calculate_merkle_root()
        self.hash = self.calculate_hash()
    
    def calculate_hash(self) -> str:
        """Hitung hash block menggunakan SHA-256"""
        block_string = json.dumps({
            "index": self.index,
            "timestamp": self.timestamp,
            "data": self.data,
            "previous_hash": self.previous_hash,
            "nonce": self.nonce,
            "merkle_root": self.merkle_root
        }, sort_keys=True)
        return hashlib.sha256(block_string.encode()).hexdigest()
    
    def calculate_merkle_root(self) -> str:
        """Hitung Merkle Root dari daftar transaksi"""
        if not self.data:
            return hashlib.sha256(b'').hexdigest()
        
        # Convert transactions to hashes
        tx_hashes = []
        for tx in self.data:
            tx_string = json.dumps(tx, sort_keys=True)
            tx_hashes.append(hashlib.sha256(tx_string.encode()).hexdigest())
        
        # Build Merkle Tree
        while len(tx_hashes) > 1:
            next_level = []
            for i in range(0, len(tx_hashes), 2):
                if i + 1 < len(tx_hashes):
                    combined = tx_hashes[i] + tx_hashes[i + 1]
                else:
                    combined = tx_hashes[i] + tx_hashes[i]
                
                next_level.append(hashlib.sha256(combined.encode()).hexdigest())
            
            tx_hashes = next_level
        
        return tx_hashes[0] if tx_hashes else hashlib.sha256(b'').hexdigest()
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert block ke dictionary untuk JSON serialization"""
        return {
            "index": self.index,
            "timestamp": self.timestamp,
            "data": self.data,
            "previous_hash": self.previous_hash,
            "nonce": self.nonce,
            "merkle_root": self.merkle_root,
            "hash": self.hash
        }
    
    def __str__(self):
        return f"Block #{self.index} - Hash: {self.hash[:16]}... - Transactions: {len(self.data)}"

class Blockchain:
    """Implementasi Blockchain dengan validasi dan consensus"""
    
    def __init__(self, difficulty: int = 4):
        self.chain = [self.create_genesis_block()]
        self.difficulty = difficulty
        self.transaction_pool = []
        self.mining_reward = 10.0
    
    def create_genesis_block(self) -> Block:
        """Buat genesis block (block pertama)"""
        genesis_data = [{
            "sender": "Genesis",
            "recipient": "Network",
            "amount": 0,
            "timestamp": time.time(),
            "tx_id": "genesis_transaction"
        }]
        return Block(0, time.time(), genesis_data, "0")
    
    def get_latest_block(self) -> Block:
        """Dapatkan block terakhir dalam chain"""
        return self.chain[-1]
    
    def add_transaction(self, transaction: Dict[str, Any]) -> bool:
        """Tambahkan transaksi ke transaction pool"""
        try:
            # Validasi basic transaksi
            required_fields = ['sender', 'recipient', 'amount', 'tx_id']
            if not all(field in transaction for field in required_fields):
                return False
            
            if transaction['amount'] <= 0:
                return False
            
            # Cek duplikasi transaksi
            existing_tx_ids = [tx['tx_id'] for tx in self.transaction_pool]
            if transaction['tx_id'] in existing_tx_ids:
                return False
            
            self.transaction_pool.append(transaction)
            return True
        except Exception as e:
            print(f"Error adding transaction: {e}")
            return False
    
    def get_balance(self, address: str) -> float:
        """Hitung balance untuk address tertentu"""
        balance = 0.0
        
        for block in self.chain:
            for tx in block.data:
                if tx['sender'] == address:
                    balance -= tx['amount']
                if tx['recipient'] == address:
                    balance += tx['amount']
        
        # Tambahkan dari transaction pool (pending)
        for tx in self.transaction_pool:
            if tx['sender'] == address:
                balance -= tx['amount']
        
        return balance
    
    def validate_transaction(self, transaction: Dict[str, Any]) -> bool:
        """Validasi transaksi berdasarkan balance"""
        if transaction['sender'] == "System":  # Mining reward
            return True
        
        sender_balance = self.get_balance(transaction['sender'])
        return sender_balance >= transaction['amount']
    
    def is_chain_valid(self, chain: Optional[List[Block]] = None) -> bool:
        """Validasi blockchain"""
        if chain is None:
            chain = self.chain
        
        for i in range(1, len(chain)):
            current_block = chain[i]
            previous_block = chain[i-1]
            
            # Cek hash block saat ini
            if current_block.hash != current_block.calculate_hash():
                print(f"Invalid hash for block {current_block.index}")
                return False
            
            # Cek link ke previous block
            if current_block.previous_hash != previous_block.hash:
                print(f"Invalid previous hash for block {current_block.index}")
                return False
            
            # Cek proof of work
            if not current_block.hash.startswith('0' * self.difficulty):
                print(f"Invalid proof of work for block {current_block.index}")
                return False
            
            # Cek merkle root
            if current_block.merkle_root != current_block.calculate_merkle_root():
                print(f"Invalid merkle root for block {current_block.index}")
                return False
        
        return True
    
    def replace_chain(self, new_chain: List[Block]) -> bool:
        """Ganti chain dengan chain yang lebih panjang dan valid"""
        if len(new_chain) > len(self.chain) and self.is_chain_valid(new_chain):
            self.chain = new_chain
            # Clear transaction pool untuk transaksi yang sudah ada di chain
            self.clear_processed_transactions()
            return True
        return False
    
    def clear_processed_transactions(self):
        """Hapus transaksi yang sudah diproses dari pool"""
        processed_tx_ids = set()
        for block in self.chain:
            for tx in block.data:
                processed_tx_ids.add(tx.get('tx_id'))
        
        self.transaction_pool = [
            tx for tx in self.transaction_pool 
            if tx.get('tx_id') not in processed_tx_ids
        ]
    
    def get_chain_info(self) -> Dict[str, Any]:
        """Dapatkan informasi lengkap tentang blockchain"""
        return {
            "length": len(self.chain),
            "difficulty": self.difficulty,
            "pending_transactions": len(self.transaction_pool),
            "latest_block_hash": self.get_latest_block().hash,
            "is_valid": self.is_chain_valid()
        }
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert blockchain ke dictionary"""
        return {
            "chain": [block.to_dict() for block in self.chain],
            "difficulty": self.difficulty,
            "transaction_pool": self.transaction_pool,
            "mining_reward": self.mining_reward
        }