from typing import Dict, Any
import hashlib
import json
from datetime import datetime

class Transaction:
    def __init__(self, sender: str, recipient: str, amount: float):
        self.sender = sender
        self.recipient = recipient
        self.amount = amount
        self.timestamp = self.get_timestamp()  # Set timestamp FIRST
        self.tx_id = self.calculate_hash()     # Then calculate hash
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "sender": self.sender,
            "recipient": self.recipient,
            "amount": self.amount,
            "tx_id": self.tx_id,
            "timestamp": self.timestamp  # Include timestamp in dict
        }
   
    def get_timestamp(self) -> str:
        return datetime.now().isoformat()
    
    def calculate_hash(self) -> str:
        tx_string = f"{self.sender}{self.recipient}{self.amount}{self.timestamp}"
        return hashlib.sha256(tx_string.encode()).hexdigest()
    
    @staticmethod
    def from_dict(data: Dict[str, Any]) -> 'Transaction':
        # Create transaction without calling __init__ to preserve original data
        tx = Transaction.__new__(Transaction)
        tx.sender = data['sender']
        tx.recipient = data['recipient']
        tx.amount = data['amount']
        tx.timestamp = data.get('timestamp', datetime.now().isoformat())
        tx.tx_id = data.get('tx_id')
        
        # If tx_id is missing, calculate it
        if not tx.tx_id:
            tx.tx_id = tx.calculate_hash()
        
        return tx
   
    def is_valid(self) -> bool:
        if not self.sender or not self.recipient:
            return False
        if self.amount <= 0:
            return False
        return True

class TransactionPool:
    def __init__(self):
        self.transactions: Dict[str, Transaction] = {}
    
    def add_transaction(self, transaction: Transaction) -> bool:
        if not transaction.is_valid():
            return False
        
        # Check if transaction already exists (prevent duplicates)
        if transaction.tx_id in self.transactions:
            return False
            
        self.transactions[transaction.tx_id] = transaction
        return True
    
    def get_transactions(self) -> Dict[str, Transaction]:
        return self.transactions
    
    def clear(self):
        self.transactions.clear()
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "transactions": [tx.to_dict() for tx in self.transactions.values()]
        }
   
    def delete_transaction(self, tx_id: str) -> bool:
        if tx_id in self.transactions:
            del self.transactions[tx_id]
            return True
        return False