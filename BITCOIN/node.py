import sys
from network import app, blockchain, peers, bootstrap_connect, peers_lock
import threading
import requests
import time
import os
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

def run_node(port=None):
    global blockchain, peers
   
    if port is None:
        port = int(os.getenv("NODE_PORT", os.getenv("DEFAULT_PORT", 5000)))
   
    my_url = f"http://localhost:{port}"
   
    def run_flask():
        app.run(host='0.0.0.0', port=port, debug=False, use_reloader=False)
   
    flask_thread = threading.Thread(target=run_flask, daemon=True)
    flask_thread.start()
   
    time.sleep(2)
   
    bootstrap_nodes = os.getenv("BOOTSTRAP_NODES", "http://localhost:5000").split(",")
    
    for node in bootstrap_nodes:
        node = node.strip()
        if node and node != my_url:
            try:
                print(f"Attempting to connect to bootstrap node: {node}")
                
                response = requests.post(f"{node}/peer", json={'peer': my_url}, timeout=10)
                if response.status_code in [200, 201]:  # Accept both new and existing
                    with peers_lock:
                        peers.add(node)
                    print(f"Successfully connected to bootstrap node: {node}")
                    
                    try:
                        time.sleep(1)  # Give time for peer list to be updated
                        peer_response = requests.get(f"{node}/peers", timeout=10)
                        if peer_response.status_code == 200:
                            peer_list = peer_response.json()
                            print(f"Received peer list from {node}: {peer_list}")
                            
                            with peers_lock:
                                initial_peer_count = len(peers)
                                for peer in peer_list:
                                    if peer != my_url:
                                        peers.add(peer)
                                        print(f"Added peer from bootstrap: {peer}")
                                
                                final_peer_count = len(peers)
                                print(f"Peer count increased from {initial_peer_count} to {final_peer_count}")
                    
                    except Exception as e:
                        print(f"Failed to get peer list from {node}: {e}")
                    
                    break  # Successfully connected to at least one bootstrap node
                    
                else:
                    print(f"Failed to connect to bootstrap node {node}: HTTP {response.status_code}")
                    
            except Exception as e:
                print(f"Exception connecting to bootstrap node {node}: {e}")
                continue
    
    with peers_lock:
        current_peers = list(peers)
    print(f"Node running on {my_url}")
    print(f"Current peers: {current_peers}")
    print(f"Total peers: {len(current_peers)}")
   
    try:
        counter = 0
        while True:
            time.sleep(10)
            counter += 1
            if counter % 6 == 0:  
                with peers_lock:
                    current_peers = list(peers)
                print(f"[{time.strftime('%H:%M:%S')}] Active peers: {len(current_peers)} - {current_peers}")
    except KeyboardInterrupt:
        print("Shutting down node...")
        sys.exit(0)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    else:
        port = None  
    run_node(port)