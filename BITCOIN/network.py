import requests
import threading
import time
import json
import logging
from flask import Flask, request, jsonify
from typing import List, Dict, Any, Set
from blockchain import Block, Blockchain
from mining import Miner
from transaction import Transaction, parse_transaction_from_dict

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class NetworkNode:
    """Node dalam Bitcoin Network untuk komunikasi antar peer"""
    
    def __init__(self, host: str, port: int, blockchain: Blockchain, miner: Miner, node_id: str = None):
        self.host = host
        self.port = port
        self.node_id = node_id or f"node_{port}"
        self.blockchain = blockchain
        self.miner = miner
        self.peers: Set[str] = set()  # Set of peer URLs
        self.app = Flask(__name__)
        self.app.logger.disabled = True  # Disable Flask logging untuk cleaner output
        
        # Network statistics
        self.stats = {
            "blocks_received": 0,
            "blocks_sent": 0,
            "transactions_received": 0,
            "sync_requests": 0,
            "connected_peers": 0
        }
        
        self.setup_routes()
        self.miner.set_callback(self.on_block_mined)
    
    def setup_routes(self):
        """Setup Flask routes untuk REST API"""
        
        @self.app.route('/info', methods=['GET'])
        def node_info():
            """Informasi node"""
            return jsonify({
                'node_id': self.node_id,
                'host': self.host,
                'port': self.port,
                'peers': list(self.peers),
                'stats': self.stats,
                'blockchain_info': self.blockchain.get_chain_info(),
                'mining_stats': self.miner.get_mining_stats()
            })
        
        @self.app.route('/chain', methods=['GET'])
        def get_chain():
            """Dapatkan seluruh blockchain"""
            try:
                chain_data = []
                for block in self.blockchain.chain:
                    chain_data.append(block.to_dict())
                
                return jsonify({
                    'chain': chain_data,
                    'length': len(chain_data),
                    'difficulty': self.blockchain.difficulty,
                    'is_valid': self.blockchain.is_chain_valid()
                })
            except Exception as e:
                logger.error(f"Error getting chain: {e}")
                return jsonify({'error': 'Failed to get chain'}), 500
        
        @self.app.route('/transaction', methods=['POST'])
        def add_transaction():
            """Tambahkan transaksi baru"""
            try:
                data = request.get_json()
                required_fields = ['sender', 'recipient', 'amount']
                
                if not all(field in data for field in required_fields):
                    return jsonify({'error': 'Missing required fields'}), 400
                
                # Buat objek transaksi
                tx = Transaction(data['sender'], data['recipient'], data['amount'])
                
                if not tx.is_valid():
                    return jsonify({'error': 'Invalid transaction'}), 400
                
                # Cek balance untuk non-system transactions
                if tx.sender != "System" and self.blockchain.get_balance(tx.sender) < tx.amount:
                    return jsonify({'error': 'Insufficient balance'}), 400
                
                # Tambahkan ke pool
                if self.blockchain.add_transaction(tx.to_dict()):
                    self.stats["transactions_received"] += 1
                    
                    # Broadcast ke peers
                    self.broadcast_transaction(tx.to_dict())
                    
                    return jsonify({
                        'message': 'Transaction added to pool',
                        'tx_id': tx.tx_id,
                        'pending_transactions': len(self.blockchain.transaction_pool)
                    })
                else:
                    return jsonify({'error': 'Failed to add transaction'}), 400
            
            except Exception as e:
                logger.error(f"Error adding transaction: {e}")
                return jsonify({'error': 'Failed to process transaction'}), 500
        
        @self.app.route('/mine', methods=['GET'])
        def mine_block():
            """Mine block baru"""
            try:
                if self.miner.is_mining:
                    return jsonify({'error': 'Already mining'}), 400
                
                if not self.blockchain.transaction_pool:
                    return jsonify({'error': 'No transactions to mine'}), 400
                
                # Mine dalam thread terpisah
                def mine_async():
                    self.miner.mine_block()
                
                mining_thread = threading.Thread(target=mine_async, daemon=True)
                mining_thread.start()
                
                return jsonify({
                    'message': 'Mining started',
                    'pending_transactions': len(self.blockchain.transaction_pool),
                    'difficulty': self.blockchain.difficulty
                })
            
            except Exception as e:
                logger.error(f"Error starting mining: {e}")
                return jsonify({'error': 'Failed to start mining'}), 500
        
        @self.app.route('/block', methods=['POST'])
        def receive_block():
            """Terima block baru dari peer"""
            try:
                block_data = request.get_json()
                
                if self.validate_and_add_block(block_data):
                    self.stats["blocks_received"] += 1
                    return jsonify({'message': 'Block accepted'})
                else:
                    return jsonify({'error': 'Block rejected'}), 400
            
            except Exception as e:
                logger.error(f"Error receiving block: {e}")
                return jsonify({'error': 'Failed to process block'}), 500
        
        @self.app.route('/consensus', methods=['GET'])
        def consensus():
            """Sinkronisasi dengan network (Longest Chain Rule)"""
            try:
                replaced = self.resolve_conflicts()
                self.stats["sync_requests"] += 1
                
                if replaced:
                    return jsonify({
                        'message': 'Chain replaced with consensus',
                        'new_length': len(self.blockchain.chain)
                    })
                else:
                    return jsonify({
                        'message': 'Chain is authoritative',
                        'length': len(self.blockchain.chain)
                    })
            
            except Exception as e:
                logger.error(f"Error in consensus: {e}")
                return jsonify({'error': 'Failed to sync'}), 500
        
        @self.app.route('/peers', methods=['GET'])
        def get_peers():
            """Dapatkan daftar peers"""
            return jsonify({
                'peers': list(self.peers),
                'count': len(self.peers)
            })
        
        @self.app.route('/peers', methods=['POST'])
        def add_peer():
            """Tambahkan peer baru"""
            try:
                data = request.get_json()
                peer_url = data.get('peer_url')
                
                if not peer_url:
                    return jsonify({'error': 'Missing peer_url'}), 400
                
                self.add_peer(peer_url)
                return jsonify({
                    'message': 'Peer added',
                    'peer_url': peer_url,
                    'total_peers': len(self.peers)
                })
            
            except Exception as e:
                logger.error(f"Error adding peer: {e}")
                return jsonify({'error': 'Failed to add peer'}), 500
        
        @self.app.route('/balance/<address>', methods=['GET'])
        def get_balance(address):
            """Dapatkan balance untuk address"""
            try:
                balance = self.blockchain.get_balance(address)
                return jsonify({
                    'address': address,
                    'balance': balance
                })
            except Exception as e:
                logger.error(f"Error getting balance: {e}")
                return jsonify({'error': 'Failed to get balance'}), 500
        
        @self.app.route('/stop_mining', methods=['POST'])
        def stop_mining():
            """Hentikan mining"""
            try:
                self.miner.stop_mining()
                return jsonify({'message': 'Mining stopped'})
            except Exception as e:
                logger.error(f"Error stopping mining: {e}")
                return jsonify({'error': 'Failed to stop mining'}), 500
    
    def validate_and_add_block(self, block_data: Dict[str, Any]) -> bool:
        """Validasi dan tambahkan block dari peer"""
        try:
            # Recreate block object
            block = Block(
                block_data['index'],
                block_data['timestamp'],
                block_data['data'],
                block_data['previous_hash'],
                block_data['nonce'],
                block_data.get('merkle_root')
            )
            
            # Validasi basic
            last_block = self.blockchain.get_latest_block()
            
            # Cek apakah block sudah ada
            for existing_block in self.blockchain.chain:
                if existing_block.hash == block.hash:
                    return False  # Block sudah ada
            
            # Validasi block
            if (block.index == last_block.index + 1 and
                block.previous_hash == last_block.hash and
                block.hash.startswith('0' * self.blockchain.difficulty) and
                block.hash == block.calculate_hash()):
                
                # Hentikan mining jika sedang berlangsung
                if self.miner.is_mining:
                    self.miner.stop_mining()
                
                self.blockchain.chain.append(block)
                
                # Hapus transaksi yang sudah diproses
                processed_tx_ids = [tx['tx_id'] for tx in block.data]
                self.blockchain.transaction_pool = [
                    tx for tx in self.blockchain.transaction_pool
                    if tx['tx_id'] not in processed_tx_ids
                ]
                
                logger.info(f"✅ Block {block.index} accepted from peer")
                return True
            
            return False
            
        except Exception as e:
            logger.error(f"Error validating block: {e}")
            return False
    
    def resolve_conflicts(self) -> bool:
        """Implementasi Longest Chain Rule"""
        new_chain = None
        max_length = len(self.blockchain.chain)
        
        for peer in self.peers:
            try:
                response = requests.get(f'{peer}/chain', timeout=10)
                if response.status_code == 200:
                    data = response.json()
                    length = data['length']
                    chain_data = data['chain']
                    
                    if length > max_length:
                        # Reconstruct blockchain
                        chain = []
                        for block_data in chain_data:
                            block = Block(
                                block_data['index'],
                                block_data['timestamp'],
                                block_data['data'],
                                block_data['previous_hash'],
                                block_data['nonce'],
                                block_data.get('merkle_root')
                            )
                            chain.append(block)
                        
                        if self.blockchain.is_chain_valid(chain):
                            max_length = length
                            new_chain = chain
                            logger.info(f"🔄 Found longer valid chain from {peer}: {length} blocks")
            
            except requests.exceptions.RequestException as e:
                logger.warning(f"Failed to sync with {peer}: {e}")
                continue
        
        if new_chain:
            self.blockchain.chain = new_chain
            self.blockchain.clear_processed_transactions()
            logger.info(f"🔄 Chain replaced with {len(new_chain)} blocks")
            return True
        
        return False
    
    def broadcast_block(self, block: Block):
        """Broadcast block ke semua peer"""
        block_data = block.to_dict()
        successful_broadcasts = 0
        
        for peer in self.peers.copy():  # Copy untuk avoid modification during iteration
            try:
                response = requests.post(
                    f'{peer}/block',
                    json=block_data,
                    timeout=5
                )
                if response.status_code == 200:
                    successful_broadcasts += 1
                    self.stats["blocks_sent"] += 1
            except requests.exceptions.RequestException as e:
                logger.warning(f"Failed to broadcast block to {peer}: {e}")
                # Remove non-responsive peer
                self.peers.discard(peer)
        
        logger.info(f"📡 Block {block.index} broadcasted to {successful_broadcasts}/{len(self.peers)} peers")
    
    def broadcast_transaction(self, transaction: Dict[str, Any]):
        """Broadcast transaksi ke semua peer"""
        for peer in self.peers.copy():
            try:
                requests.post(
                    f'{peer}/transaction',
                    json=transaction,
                    timeout=5
                )
            except requests.exceptions.RequestException:
                # Silently ignore failed transaction broadcasts
                pass
    
    def on_block_mined(self, block: Block):
        """Callback ketika berhasil mining block"""
        logger.info(f"🎉 Block {block.index} mined successfully!")
        self.broadcast_block(block)
    
    def add_peer(self, peer_url: str):
        """Tambahkan peer baru"""
        if peer_url != f"http://{self.host}:{self.port}":  # Jangan tambahkan diri sendiri
            self.peers.add(peer_url)
            self.stats["connected_peers"] = len(self.peers)
            logger.info(f"🤝 Added peer: {peer_url}")
    
    def remove_peer(self, peer_url: str):
        """Hapus peer"""
        if peer_url in self.peers:
            self.peers.remove(peer_url)
            self.stats["connected_peers"] = len(self.peers)
            logger.info(f"❌ Removed peer: {peer_url}")
    
    def discover_peers(self):
        """Discover peers dari peer yang sudah ada"""
        discovered_peers = set()
        
        for peer in self.peers.copy():
            try:
                response = requests.get(f'{peer}/peers', timeout=5)
                if response.status_code == 200:
                    data = response.json()
                    for peer_url in data['peers']:
                        if peer_url not in self.peers and peer_url != f"http://{self.host}:{self.port}":
                            discovered_peers.add(peer_url)
            except requests.exceptions.RequestException:
                continue
        
        # Tambahkan discovered peers
        for peer_url in discovered_peers:
            self.add_peer(peer_url)
        
        logger.info(f"🔍 Discovered {len(discovered_peers)} new peers")
    
    def start_auto_sync(self, interval: int = 30):
        """Mulai auto sync dengan interval tertentu"""
        def sync_loop():
            while True:
                try:
                    time.sleep(interval)
                    self.resolve_conflicts()
                    self.discover_peers()
                except Exception as e:
                    logger.error(f"Error in auto sync: {e}")
        
        sync_thread = threading.Thread(target=sync_loop, daemon=True)
        sync_thread.start()
        logger.info(f"🔄 Auto sync started with {interval}s interval")
    
    def run(self, debug: bool = False, auto_sync: bool = True):
        """Jalankan node"""
        if auto_sync:
            self.start_auto_sync()
        
        logger.info(f"🚀 Starting Bitcoin Node {self.node_id}")
        logger.info(f"🌐 Address: http://{self.host}:{self.port}")
        logger.info(f"👥 Peers: {list(self.peers)}")
        
        try:
            self.app.run(
                host=self.host,
                port=self.port,
                threaded=True,
                debug=debug
            )
        except Exception as e:
            logger.error(f"Error running node: {e}")
    
    def get_network_stats(self) -> Dict[str, Any]:
        """Dapatkan statistik network"""
        return {
            "node_id": self.node_id,
            "network_stats": self.stats,
            "peer_count": len(self.peers),
            "peers": list(self.peers),
            "blockchain_length": len(self.blockchain.chain),
            "pending_transactions": len(self.blockchain.transaction_pool),
            "is_mining": self.miner.is_mining
        }