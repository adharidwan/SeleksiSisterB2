from flask import Flask, jsonify, request
import requests
from blockchain import Blockchain, Block
from transaction import Transaction
import socket
import threading
import json
import time
import os
from dotenv import load_dotenv

load_dotenv()

app = Flask(__name__)

difficulty = int(os.getenv("DEFAULT_DIFFICULTY", 4))  
mining_reward = 10  
blockchain = Blockchain(difficulty=difficulty)


seen_transactions = set() 
seen_transactions_lock = threading.Lock()

peers = set()
peers_lock = threading.Lock()  
MULTICAST_GROUP = '224.0.0.1'
MULTICAST_PORT = 5001
ANNOUNCE_INTERVAL = 10  

SYNC_INTERVAL = int(os.getenv("SYNC_INTERVAL", 10))

if not hasattr(Blockchain, 'validate_block'):
    def validate_block(self, block):
        if block.hash != block.calculate_hash():
            return False
        if block.merkle_root != block.calculate_merkle_root():
            return False
        if block.hash[:self.difficulty] != '0' * self.difficulty:
            return False
        return True
    
    Blockchain.validate_block = validate_block

def validate_chain(chain):
    for i in range(1, len(chain)):
        current = chain[i]
        previous = chain[i-1]
        if current.previous_hash != previous.hash:
            return False
        if current.hash != current.calculate_hash():
            return False
        if current.merkle_root != current.calculate_merkle_root():
            return False
    return True

@app.route('/chain', methods=['GET'])
def get_chain():
    return jsonify(blockchain.to_dict()), 200

@app.route('/transaction', methods=['POST'])
def add_transaction():
    data = request.get_json()
    if not data or not all(key in data for key in ('sender', 'recipient', 'amount')):
        return jsonify({'message': 'Missing fields'}), 400
    
    is_broadcast = 'tx_id' in data and 'timestamp' in data
    
    if is_broadcast:
        tx = Transaction.from_dict(data)
        print(f"📩 Received broadcast: {tx.tx_id[:8]} from {tx.sender} -> {tx.recipient} ({tx.amount})")
    else:
        tx = Transaction(data['sender'], data['recipient'], data['amount'])
        print(f"🆕 New transaction: {tx.tx_id[:8]} from {tx.sender} -> {tx.recipient} ({tx.amount})")
    
    if tx.tx_id in blockchain.transaction_pool.transactions:
        print(f"⚠️  Transaction {tx.tx_id[:8]} already exists, skipping")
        return jsonify({'message': 'Transaction already exists'}), 200
    
    if blockchain.transaction_pool.add_transaction(tx, blockchain):
        if not is_broadcast:
            print(f"📡 Broadcasting transaction: {tx.tx_id[:8]}")
            broadcast_transaction(tx)
        
        return jsonify({'message': 'Transaction added', 'tx_id': tx.tx_id}), 201
    else:
        if tx.sender != "Network":  # Don't check balance for mining rewards
            balance = blockchain.get_balance(tx.sender)
            return jsonify({
                'message': 'Invalid transaction - insufficient balance',
                'balance': balance,
                'required': tx.amount
            }), 400
        return jsonify({'message': 'Invalid transaction'}), 400

def broadcast_transaction(tx):
    tx_dict = tx.to_dict()  
    
    with peers_lock:
        current_peers = list(peers)
    
    print(f"📡 Broadcasting to {len(current_peers)} peers: {tx.tx_id[:8]}")
    
    for peer in current_peers:
        try:
            response = requests.post(f"{peer}/transaction", json=tx_dict, timeout=5)
            print(f"  ✅ {peer}: {response.status_code}")
        except Exception as e:
            print(f"  ❌ {peer}: {e}")
            pass

@app.route('/peers', methods=['GET']) 
def get_peers():
    with peers_lock:
        return jsonify(list(peers)), 200

@app.route('/peer', methods=['POST'])
def add_peer():
    data = request.get_json()
    if not data or 'peer' not in data:
        return jsonify({'message': 'Missing peer URL'}), 400
    
    peer = data['peer']
    my_url = request.url_root.rstrip('/')
    propagate = data.get('propagate', True)  
    
    if peer != my_url:
        with peers_lock:
            if peer not in peers:
                peers.add(peer)
                print(f"Added new peer: {peer}")
                
                if propagate:
                    current_peers = list(peers)
                    for existing_peer in current_peers:
                        if existing_peer != peer:
                            try:
                                requests.post(f"{existing_peer}/peer", 
                                            json={'peer': peer, 'propagate': False}, 
                                            timeout=5)
                            except:
                                pass
                    
                    # Tell the new peer about all existing peers (without propagation)
                    for existing_peer in current_peers:
                        if existing_peer != peer:
                            try:
                                requests.post(f"{peer}/peer", 
                                            json={'peer': existing_peer, 'propagate': False}, 
                                            timeout=5)
                            except:
                                pass
                
                return jsonify({'message': 'Peer added'}), 201
            else:
                return jsonify({'message': 'Peer already exists'}), 200
    
    return jsonify({'message': 'Invalid peer'}), 400

@app.route('/balance/<account>', methods=['GET'])
def get_balance(account):
    balance = blockchain.get_balance(account)
    return jsonify({'account_name' : account ,'balance': balance}), 200

@app.route('/mine', methods=['GET'])
def mine():
    miner_address = request.args.get('miner_address')
    if not miner_address:
        return jsonify({'message': 'Miner address required'}), 400
    mined_block = blockchain.mine_block(miner_address)
    if mined_block:
        broadcast_block(mined_block)
        return jsonify(mined_block.to_dict()), 201
    return jsonify({'message': 'Mining failed'}), 400

def broadcast_block(block):
    block_dict = block.to_dict()
    my_url = request.url_root.rstrip('/')
    data = {'block': block_dict, 'sender': my_url}
    
    with peers_lock:
        current_peers = list(peers)
    
    for peer in current_peers:
        try:
            response = requests.post(f"{peer}/block", json=data, timeout=5)
            if response.status_code == 400:  # If peer rejects, perhaps sync
                sync_chain(peer)
        except:
            pass
@app.route('/block', methods=['POST'])
def receive_block():
    data = request.get_json()
    if not data or 'block' not in data or 'sender' not in data:
        return jsonify({'message': 'Missing data'}), 400
    
    block_dict = data['block']
    block = Block(
        index=block_dict['index'],
        timestamp=block_dict['timestamp'],
        data=block_dict['data'],
        previous_hash=block_dict['previous_hash'],
        nonce=block_dict['nonce'],
        merkle_root=block_dict['merkle_root']
    )
    
    if block.hash != block.calculate_hash():
        return jsonify({'message': 'Invalid hash'}), 400
    
    latest = blockchain.get_latest_block()
    if block.previous_hash == latest.hash and block.index == latest.index + 1 and blockchain.validate_block(block):
        blockchain.add_block(block)
        
        print(f"📦 Received block #{block.index} with {len(block.data)} transactions")
        removed_count = 0
        for tx_data in block.data:
            if 'tx_id' in tx_data and blockchain.transaction_pool.delete_transaction(tx_data['tx_id']):
                removed_count += 1
        
        print(f"🧹 Removed {removed_count} transactions from pool")
        print(f"📊 Pool now has {len(blockchain.transaction_pool.get_transactions())} transactions")
        
        return jsonify({'message': 'Block added'}), 201
    else:
        sync_chain(data['sender'])
        return jsonify({'message': 'Chain synced'}), 200

def sync_chain(sender_url):
    try:
        response = requests.get(f"{sender_url}/chain", timeout=5)
        if response.status_code == 200:
            data = response.json()
            new_chain_data = data['chain']
            new_chain = []
            for b in new_chain_data:
                block = Block(
                    index=b['index'],
                    timestamp=b['timestamp'],
                    data=b['data'],
                    previous_hash=b['previous_hash'],
                    nonce=b['nonce'],
                    merkle_root=b['merkle_root']
                )
                new_chain.append(block)
            if len(new_chain) > len(blockchain.chain) and validate_chain(new_chain):
                print(f"🔄 Syncing to longer chain from {sender_url}")
                blockchain.chain = new_chain
                
                removed_count = 0
                for block in blockchain.chain[1:]:  
                    for tx_data in block.data:
                        if 'tx_id' in tx_data and blockchain.transaction_pool.delete_transaction(tx_data['tx_id']):
                            removed_count += 1
                
                print(f"🧹 Cleaned up {removed_count} transactions from pool after sync")
                print(f"📊 Pool now has {len(blockchain.transaction_pool.get_transactions())} transactions")
    except Exception as e:
        print(f"Sync error: {e}")
        pass

def announce_self(my_url):
    while True:
        message = json.dumps({'peer': my_url, 'type': 'announcement'})
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
            sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, 2)
            sock.sendto(message.encode(), (MULTICAST_GROUP, MULTICAST_PORT))
            sock.close()
        except Exception as e:
            print(f"Multicast announcement error: {e}")
        time.sleep(ANNOUNCE_INTERVAL)

def listen_for_peers(my_url):
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind(('', MULTICAST_PORT))
        mreq = socket.inet_aton(MULTICAST_GROUP) + socket.inet_aton('0.0.0.0')
        sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
        
        while True:
            data, addr = sock.recvfrom(1024)
            try:
                message = json.loads(data.decode())
                peer = message.get('peer')
                if peer and peer != my_url:
                    with peers_lock:
                        if peer not in peers:
                            peers.add(peer)
                            print(f"Discovered peer via multicast: {peer}")
                    
                    # Try to establish bidirectional connection (without propagation to avoid multicast loops)
                    try:
                        requests.post(f"{peer}/peer", 
                                    json={'peer': my_url, 'propagate': False}, 
                                    timeout=5)
                    except:
                        pass
            except Exception as e:
                print(f"Error processing multicast message: {e}")
    except Exception as e:
        print(f"Multicast listener error: {e}")

def periodic_sync():
    while True:
        with peers_lock:
            current_peers = list(peers)
        
        for peer in current_peers:
            try:
                sync_chain(peer)
            except:
                # Remove dead peers
                with peers_lock:
                    peers.discard(peer)
        time.sleep(SYNC_INTERVAL)

def bootstrap_connect(my_url, bootstrap_nodes):
    for node in bootstrap_nodes:
        node = node.strip()
        if node and node != my_url:
            try:
                # Add ourselves to the bootstrap node
                response = requests.post(f"{node}/peer", json={'peer': my_url}, timeout=5)
                if response.status_code in [200, 201]:  # Accept both new and existing
                    with peers_lock:
                        peers.add(node)
                    print(f"Connected to bootstrap node: {node}")
                    
                    # Get the peer list from bootstrap node (but don't auto-connect to avoid loops)
                    try:
                        peer_response = requests.get(f"{node}/peers", timeout=5)
                        if peer_response.status_code == 200:
                            peer_list = peer_response.json()
                            with peers_lock:
                                for peer in peer_list:
                                    if peer != my_url:
                                        peers.add(peer)
                                        print(f"Added peer from bootstrap: {peer}")
                    except Exception as e:
                        print(f"Failed to get peer list from {node}: {e}")
                    
                    break
            except Exception as e:
                print(f"Failed to connect to bootstrap node {node}: {e}")
                continue

if __name__ == '__main__':
    import sys
    port = int(os.getenv("DEFAULT_PORT", 5000)) if len(sys.argv) <= 1 else int(sys.argv[1])
    my_url = f"http://localhost:{port}"
    
    threading.Thread(target=listen_for_peers, args=(my_url,), daemon=True).start()
    threading.Thread(target=announce_self, args=(my_url,), daemon=True).start()
    threading.Thread(target=periodic_sync, daemon=True).start()
    
    bootstrap_nodes = os.getenv("BOOTSTRAP_NODES", "http://localhost:5000").split(",")
    if bootstrap_nodes:
        threading.Thread(target=bootstrap_connect, args=(my_url, bootstrap_nodes), daemon=True).start()
    
    app.run(host='0.0.0.0', port=port, debug=True)