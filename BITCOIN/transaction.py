import hashlib
import json
import time
import uuid
from typing import Dict, Any, Optional

class Transaction:
    """Implementasi Bitcoin Transaction dengan validasi"""
    
    def __init__(self, sender: str, recipient: str, amount: float, 
                 timestamp: Optional[float] = None, tx_id: Optional[str] = None):
        self.sender = sender
        self.recipient = recipient
        self.amount = float(amount)
        self.timestamp = timestamp or time.time()
        self.tx_id = tx_id or self.generate_tx_id()
    
    def generate_tx_id(self) -> str:
        """Generate unique transaction ID"""
        # Kombinasi timestamp, sender, recipient, amount, dan UUID untuk uniqueness
        unique_string = f"{self.timestamp}{self.sender}{self.recipient}{self.amount}{uuid.uuid4()}"
        return hashlib.sha256(unique_string.encode()).hexdigest()
    
    def calculate_hash(self) -> str:
        """Hitung hash transaksi untuk validasi"""
        tx_string = json.dumps({
            "tx_id": self.tx_id,
            "sender": self.sender,
            "recipient": self.recipient,
            "amount": self.amount,
            "timestamp": self.timestamp
        }, sort_keys=True)
        return hashlib.sha256(tx_string.encode()).hexdigest()
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert transaction ke dictionary"""
        return {
            "tx_id": self.tx_id,
            "sender": self.sender,
            "recipient": self.recipient,
            "amount": self.amount,
            "timestamp": self.timestamp,
            "hash": self.calculate_hash()
        }
    
    def is_valid(self) -> bool:
        """Validasi basic transaction"""
        try:
            # Cek field yang required
            if not all([self.sender, self.recipient, self.tx_id]):
                return False
            
            # Cek amount harus positif
            if self.amount <= 0:
                return False
            
            # Cek timestamp valid
            if self.timestamp <= 0:
                return False
            
            # Sender dan recipient tidak boleh sama (kecuali untuk mining reward)
            if self.sender == self.recipient and self.sender != "System":
                return False
            
            return True
        except Exception as e:
            print(f"Transaction validation error: {e}")
            return False
    
    def __str__(self):
        return f"TX({self.tx_id[:8]}...): {self.sender} -> {self.recipient} = {self.amount}"
    
    def __eq__(self, other):
        if not isinstance(other, Transaction):
            return False
        return self.tx_id == other.tx_id
    
    def __hash__(self):
        return hash(self.tx_id)

class TransactionPool:
    """Pool untuk menampung transaksi yang belum diproses"""
    
    def __init__(self, max_size: int = 1000):
        self.transactions = []
        self.max_size = max_size
        self.tx_ids = set()  # Untuk cek duplikasi cepat
    
    def add_transaction(self, transaction: Transaction) -> bool:
        """Tambahkan transaksi ke pool"""
        try:
            # Cek validitas transaksi
            if not transaction.is_valid():
                print(f"Invalid transaction: {transaction}")
                return False
            
            # Cek duplikasi
            if transaction.tx_id in self.tx_ids:
                print(f"Duplicate transaction: {transaction.tx_id}")
                return False
            
            # Cek kapasitas pool
            if len(self.transactions) >= self.max_size:
                print("Transaction pool is full")
                return False
            
            self.transactions.append(transaction)
            self.tx_ids.add(transaction.tx_id)
            return True
        
        except Exception as e:
            print(f"Error adding transaction to pool: {e}")
            return False
    
    def remove_transaction(self, tx_id: str) -> bool:
        """Hapus transaksi dari pool"""
        try:
            for i, tx in enumerate(self.transactions):
                if tx.tx_id == tx_id:
                    self.transactions.pop(i)
                    self.tx_ids.remove(tx_id)
                    return True
            return False
        except Exception as e:
            print(f"Error removing transaction: {e}")
            return False
    
    def get_transactions(self, max_count: int = None) -> list:
        """Ambil transaksi dari pool untuk mining"""
        if max_count is None:
            return self.transactions.copy()
        return self.transactions[:max_count].copy()
    
    def clear_transactions(self, tx_ids_to_remove: list):
        """Hapus transaksi yang sudah diproses"""
        try:
            remaining_transactions = []
            remaining_ids = set()
            
            for tx in self.transactions:
                if tx.tx_id not in tx_ids_to_remove:
                    remaining_transactions.append(tx)
                    remaining_ids.add(tx.tx_id)
            
            self.transactions = remaining_transactions
            self.tx_ids = remaining_ids
        except Exception as e:
            print(f"Error clearing transactions: {e}")
    
    def get_transaction_by_id(self, tx_id: str) -> Optional[Transaction]:
        """Cari transaksi berdasarkan ID"""
        for tx in self.transactions:
            if tx.tx_id == tx_id:
                return tx
        return None
    
    def size(self) -> int:
        """Jumlah transaksi dalam pool"""
        return len(self.transactions)
    
    def is_empty(self) -> bool:
        """Cek apakah pool kosong"""
        return len(self.transactions) == 0
    
    def to_dict_list(self) -> list:
        """Convert semua transaksi ke list of dict"""
        return [tx.to_dict() for tx in self.transactions]

def create_mining_reward_transaction(miner_address: str, reward_amount: float) -> Transaction:
    """Buat transaksi reward untuk miner"""
    return Transaction(
        sender="System",
        recipient=miner_address,
        amount=reward_amount,
        timestamp=time.time()
    )

def create_genesis_transaction() -> Transaction:
    """Buat transaksi untuk genesis block"""
    return Transaction(
        sender="Genesis",
        recipient="Network",
        amount=0,
        timestamp=0,
        tx_id="genesis_transaction"
    )

# Utility functions untuk transaksi
def validate_transaction_format(tx_data: dict) -> bool:
    """Validasi format data transaksi"""
    required_fields = ['sender', 'recipient', 'amount']
    return all(field in tx_data for field in required_fields)

def parse_transaction_from_dict(tx_data: dict) -> Optional[Transaction]:
    """Parse transaksi dari dictionary"""
    try:
        return Transaction(
            sender=tx_data['sender'],
            recipient=tx_data['recipient'],
            amount=tx_data['amount'],
            timestamp=tx_data.get('timestamp'),
            tx_id=tx_data.get('tx_id')
        )
    except Exception as e:
        print(f"Error parsing transaction: {e}")
        return None