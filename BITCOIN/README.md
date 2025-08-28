# Bitcoin Blockchain Implementation

A simple blockchain implementation in Python with peer-to-peer networking, transaction pooling, and proof-of-work mining.

## Features

- **Blockchain Core**: Complete blockchain with blocks, transactions, and proof-of-work consensus
- **Transaction Pool**: Pending transaction management with balance validation
- **Mining System**: Proof-of-work mining with configurable difficulty and mining rewards
- **Peer-to-Peer Network**: Distributed network with automatic peer discovery via multicast
- **REST API**: Complete HTTP API for blockchain interactions
- **Merkle Tree**: Transaction integrity verification using Merkle roots
- **Chain Validation**: Full blockchain validation and synchronization

## Architecture

### Core Components

- **`blockchain.py`**: Core blockchain logic, blocks, and chain management
- **`transaction.py`**: Transaction handling and transaction pool management
- **`node.py`**: Network node with REST API and peer-to-peer communication
- **`config.py`**: Configuration setup and environment file generation

### Key Classes

- **`Block`**: Individual blockchain blocks with Merkle root calculation
- **`Blockchain`**: Main blockchain with mining and validation
- **`Transaction`**: Individual transactions with digital signatures
- **`TransactionPool`**: Manages pending transactions before mining

## Installation

### Prerequisites

- Python 3.7+
- pip package manager

### Setup

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd BITCOIN
   ```

2. **Install dependencies**
   ```bash
   pip install -r requirement.txt
   ```

3. **Initialize configuration**
   ```bash
   python config.py
   ```
   This creates the `.env` file with default settings.

## Usage

### Starting a Node

Start the first node (bootstrap node):
```bash
python node.py
```

Start additional nodes on different ports:
```bash
python node.py 5001
python node.py 5002
```

### API Endpoints

#### Blockchain Operations

- **Get blockchain**: `GET /chain`
  ```bash
  curl http://localhost:5000/chain
  ```

- **Mine a block**: `GET /mine?miner_address=<address>`
  ```bash
  curl "http://localhost:5000/mine?miner_address=miner1"
  ```

#### Transaction Operations

- **Add transaction**: `POST /transaction`
  ```bash
  curl -X POST http://localhost:5000/transaction \
    -H "Content-Type: application/json" \
    -d '{
      "sender": "alice",
      "recipient": "bob", 
      "amount": 10.5
    }'
  ```

- **Get balance**: `GET /balance/<account>`
  ```bash
  curl http://localhost:5000/balance/alice
  ```

#### Network Operations

- **Get peers**: `GET /peers`
  ```bash
  curl http://localhost:5000/peers
  ```

- **Add peer**: `POST /peer`
  ```bash
  curl -X POST http://localhost:5000/peer \
    -H "Content-Type: application/json" \
    -d '{"peer": "http://localhost:5001"}'
  ```

## Configuration

The `.env` file contains the following settings:

- **`DEFAULT_DIFFICULTY`**: Mining difficulty (default: 3)
- **`DEFAULT_PORT`**: Default node port (default: 5000)
- **`SYNC_INTERVAL`**: Chain synchronization interval in seconds (default: 10)
- **`GENESIS_TIMESTAMP`**: Genesis block timestamp
- **`BOOTSTRAP_NODES`**: Comma-separated list of bootstrap nodes

## Network Architecture

### Peer Discovery

The network uses multiple peer discovery mechanisms:

1. **Multicast Discovery**: Nodes announce themselves via UDP multicast
2. **Bootstrap Nodes**: Connect to known bootstrap nodes
3. **Peer Propagation**: New peers are shared across the network

### Synchronization

- Automatic chain synchronization every 10 seconds
- Longest valid chain consensus
- Transaction pool cleanup after block reception

## Mining Process

1. **Transaction Collection**: Gather transactions from the pool
2. **Reward Addition**: Add mining reward transaction
3. **Proof-of-Work**: Find nonce that satisfies difficulty requirement
4. **Block Creation**: Create block with Merkle root
5. **Broadcasting**: Share mined block with network peers

## Example Workflow

### 1. Start Network
```bash
# Terminal 1 - Bootstrap node
python node.py

# Terminal 2 - Second node
python node.py 5001

# Terminal 3 - Third node  
python node.py 5002
```

### 2. Create Transactions
```bash
# Add some transactions
curl -X POST http://localhost:5000/transaction \
  -H "Content-Type: application/json" \
  -d '{"sender": "alice", "recipient": "bob", "amount": 25}'

curl -X POST http://localhost:5001/transaction \
  -H "Content-Type: application/json" \
  -d '{"sender": "bob", "recipient": "charlie", "amount": 15}'
```

### 3. Mine Blocks
```bash
# Mine on different nodes
curl "http://localhost:5000/mine?miner_address=miner1"
curl "http://localhost:5001/mine?miner_address=miner2"
```

### 4. Check Balances
```bash
curl http://localhost:5000/balance/alice
curl http://localhost:5000/balance/bob
curl http://localhost:5000/balance/miner1
```

## Development

### Project Structure
```
BITCOIN/
├── blockchain.py      # Core blockchain implementation
├── transaction.py     # Transaction and pool management
├── node.py           # Network node and API
├── config.py         # Configuration setup
├── requirement.txt   # Python dependencies
├── .env             # Environment variables (auto-generated)
├── .gitignore       # Git ignore rules
└── README.md        # This file
```

### Key Features Implementation

- **Merkle Trees**: Efficient transaction verification
- **Proof-of-Work**: SHA-256 based mining with adjustable difficulty
- **UTXO-like Model**: Balance calculation from transaction history
- **Network Resilience**: Automatic peer discovery and chain sync
- **Transaction Validation**: Balance checking and double-spend prevention

## Troubleshooting

### Common Issues

1. **Port Already in Use**
   ```bash
   # Use a different port
   python node.py 5010
   ```

2. **Peer Connection Issues**
   ```bash
   # Manually add peers
   curl -X POST http://localhost:5000/peer \
     -H "Content-Type: application/json" \
     -d '{"peer": "http://localhost:5001"}'
   ```

3. **Mining Fails**
   - Ensure there are transactions in the pool
   - Check that the miner address is provided

### Debugging

Enable debug mode by checking the Flask debug output in the console. All major operations (transactions, mining, peer connections) are logged with emoji indicators:

- 📩 Received broadcast transaction
- 🆕 New local transaction  
- 📡 Broadcasting to peers
- 📦 Block received
- 🧹 Transaction pool cleanup
- 🔄 Chain synchronization

## Limitations

This is a simplified blockchain implementation for educational purposes:

- No digital signatures or cryptographic security
- Simplified consensus mechanism
- No persistent storage (data lost on restart)
- Limited scalability
- No transaction fees beyond mining rewards

## License

This project is for educational purposes. Please ensure proper licensing for production use.

## Video!
https://youtu.be/LoQa5sim8XQ