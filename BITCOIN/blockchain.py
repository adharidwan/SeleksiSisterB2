from typing import List, Dict, Any
import hashlib
import json
from transaction import Transaction, TransactionPool
import datetime

class Block:    
    def __init__(self, index: int, timestamp: str, data: list, previous_hash: str, nonce: int = 0, merkle_root: str = None):
        self.index = index
        self.timestamp = timestamp
        self.data = data  
        self.previous_hash = previous_hash
        self.nonce = nonce
        self.merkle_root = merkle_root or self.calculate_merkle_root()
        self.hash = self.calculate_hash()
    
    def calculate_merkle_root(self) -> str:
        if not self.data:
            return hashlib.sha256(b'').hexdigest()
        
        tx_hashes = [hashlib.sha256(json.dumps(tx, sort_keys=True, separators=(',', ':')).encode()).hexdigest() for tx in self.data]
        
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
        
    def calculate_hash(self) -> str:
        data_string = json.dumps(self.data, sort_keys=True, separators=(',', ':'))
        block_string = f"{self.index}{self.timestamp}{data_string}{self.previous_hash}{self.nonce}{self.merkle_root}"
        return hashlib.sha256(block_string.encode()).hexdigest()

    def to_dict(self) -> Dict[str, Any]:
        return {
            "index": self.index,
            "timestamp": self.timestamp,
            "data": self.data,
            "previous_hash": self.previous_hash,
            "nonce": self.nonce,
            "merkle_root": self.merkle_root,
            "hash": self.hash
        }
    
class Blockchain:
    def __init__(self, difficulty: int = 4):
        self.chain: List[Block] = []
        self.create_genesis_block()
        self.transaction_pool = TransactionPool()
        self.difficulty = difficulty 
        self.mining_reward = 10
    
    def create_genesis_block(self):
        with open(".env", "r") as file:
            lines = file.readlines()
            genesis_timestamp_str = None
            for line in lines:
                if line.startswith("GENESIS_TIMESTAMP="):
                    genesis_timestamp_str = line.split("=", 1)[1].strip()
                    break

        genesis_block = Block(
            index=0,
            timestamp=genesis_timestamp_str,
            data=[],
            previous_hash="0",
            nonce=0
        )
        self.chain.append(genesis_block)
        return genesis_block
    
    def add_block(self, block: Block):
        if not self.validate_block(block):
            print("Invalid block. Cannot add to chain.")
            return False
        
        if self.chain and block.previous_hash != self.chain[-1].hash:
            return False

        self.chain.append(block)
        return True
    
    def get_latest_block(self) -> Block:
        return self.chain[-1] if self.chain else None
    
    def get_balance(self, address: str) -> float:
        balance = 0.0
        for block in self.chain:
            for tx in block.data:
                if tx['recipient'] == address:
                    balance += tx['amount']
                elif tx['sender'] == address:
                    balance -= tx['amount']
        return balance
    
    def validate_chain(self) -> bool:
        for i in range(1, len(self.chain)):
            current_block = self.chain[i]
            previous_block = self.chain[i - 1]
            
            if current_block.previous_hash != previous_block.hash:
                return False
            
            if current_block.hash != current_block.calculate_hash():
                return False
            
            if not current_block.merkle_root or current_block.merkle_root != current_block.calculate_merkle_root():
                return False
            
        return True
    
    def mine_block(self, miner_address: str) -> Block:
        if not self.transaction_pool.get_transactions():
            print("No transactions to mine.")
            return None

        reward_tx = Transaction(
            sender="Network",
            recipient=miner_address,
            amount=self.mining_reward
        )

        pool_transactions = self.transaction_pool.get_transactions()
        
        transaction_list = [tx.to_dict() for tx in pool_transactions.values()]

        transactions = transaction_list + [reward_tx.to_dict()]

        latest_block = self.get_latest_block()
        new_index = latest_block.index + 1
        new_timestamp = datetime.datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ")
        
        new_block = Block(
            index=new_index,
            timestamp=new_timestamp,
            data=transactions,  
            previous_hash=latest_block.hash
        )

        while new_block.hash[:self.difficulty] != '0' * self.difficulty:
            new_block.nonce += 1
            new_block.hash = new_block.calculate_hash()

        if self.add_block(new_block):
            self.transaction_pool.clear()
            print(f"Block #{new_index} mined with hash: {new_block.hash}")
            print(f"Transaction pool cleared. Remaining transactions: {len(self.transaction_pool.get_transactions())}")
            return new_block
        else:
            print("Failed to add block to chain")
            return None

    def validate_block(self, block: Block) -> bool:
        if block.hash != block.calculate_hash():
            print("Invalid block hash")
            return False
        
        if block.merkle_root != block.calculate_merkle_root():
            print("Invalid merkle root")
            return False
        
        if block.hash[:self.difficulty] != '0' * self.difficulty:
            print("Block doesn't meet difficulty requirement")
            return False
        
        if self.chain and block.previous_hash != self.get_latest_block().hash:
            print("Invalid previous hash")
            return False
        
        return True

    def replace_chain(self, new_chain: List[Block]) -> bool:
        if len(new_chain) <= len(self.chain):
            return False
        
        if not self.validate_chain():
            return False
        
        self.chain = new_chain
        return True
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "chain": [block.to_dict() for block in self.chain],
            "difficulty": self.difficulty,
            "transaction_pool": self.transaction_pool.to_dict(),
            "mining_reward": self.mining_reward
        }