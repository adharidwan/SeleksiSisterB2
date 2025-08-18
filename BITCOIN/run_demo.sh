#!/bin/bash

# Bitcoin Network Demo Script
# Script untuk menjalankan demo Bitcoin network dengan mudah
# Fixed version for externally-managed Python environments

echo "🎬 Bitcoin Network Demo Setup"
echo "======================================"

# Check if Python is available
if ! command -v python3 &> /dev/null; then
    echo "❌ Python3 is required but not installed."
    exit 1
fi

# Virtual environment setup
VENV_DIR="bitcoin_demo_venv"

setup_venv() {
    echo "🐍 Setting up Python virtual environment..."
    
    # Check if venv module is available
    if ! python3 -m venv --help &> /dev/null; then
        echo "❌ python3-venv is not installed. Please install it first:"
        echo "   sudo apt install python3-venv"
        exit 1
    fi
    
    # Create virtual environment if it doesn't exist
    if [ ! -d "$VENV_DIR" ]; then
        echo "📦 Creating virtual environment..."
        python3 -m venv "$VENV_DIR"
        if [ $? -ne 0 ]; then
            echo "❌ Failed to create virtual environment"
            exit 1
        fi
    fi
    
    # Activate virtual environment
    source "$VENV_DIR/bin/activate"
    
    # Upgrade pip
    echo "⬆️  Upgrading pip..."
    pip install --upgrade pip
    
    # Install requirements if requirements.txt exists
    if [ -f "requirements.txt" ]; then
        echo "📦 Installing requirements from requirements.txt..."
        pip install -r requirements.txt
    else
        echo "⚠️  requirements.txt not found. Installing common packages..."
        # Install common packages that might be needed for a Bitcoin demo
        pip install flask requests hashlib-compat
    fi
    
    echo "✅ Virtual environment setup complete!"
}

# Function to start a node with virtual environment
start_node() {
    local node_id=$1
    local auto_mine=$2
    local title="Bitcoin Node $node_id"
    
    if [[ "$auto_mine" == "true" ]]; then
        title="$title (Auto-Mining)"
    fi
    
    echo "🚀 Starting Node $node_id..."
    
    # Command to run with virtual environment
    local cmd="cd '$PWD' && source '$VENV_DIR/bin/activate' && echo '$title' && python3 main.py $node_id"
    if [[ "$auto_mine" == "true" ]]; then
        cmd="$cmd --auto-mine"
    fi
    
    # For different operating systems
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        osascript -e "tell application \"Terminal\" to do script \"$cmd\""
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        # Linux
        if command -v gnome-terminal &> /dev/null; then
            gnome-terminal --title="$title" -- bash -c "$cmd; exec bash"
        elif command -v xterm &> /dev/null; then
            xterm -title "$title" -e "bash -c '$cmd; bash'" &
        elif command -v konsole &> /dev/null; then
            konsole --title "$title" -e bash -c "$cmd; bash" &
        elif command -v terminator &> /dev/null; then
            terminator --title="$title" -e "bash -c '$cmd; bash'" &
        else
            echo "No supported terminal found. Please run manually:"
            echo "source $VENV_DIR/bin/activate && python3 main.py $node_id --auto-mine"
        fi
    elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "win32" ]]; then
        # Windows
        start cmd.exe /k "cd /d $PWD && $VENV_DIR\Scripts\activate && echo $title && python main.py $node_id --auto-mine"
    else
        echo "Please run manually:"
        echo "source $VENV_DIR/bin/activate && python3 main.py $node_id --auto-mine"
    fi
    
    sleep 2  # Give time between node startups
}

# Function to show manual instructions
show_manual_instructions() {
    echo ""
    echo "🖥️  MANUAL SETUP INSTRUCTIONS"
    echo "=================================="
    echo "If automatic terminal opening doesn't work, please open 3 separate terminals and run:"
    echo ""
    echo "First, activate the virtual environment in each terminal:"
    echo "source $VENV_DIR/bin/activate"
    echo ""
    echo "Then run:"
    echo "Terminal 1: python3 main.py 1 --auto-mine"
    echo "Terminal 2: python3 main.py 2 --auto-mine"
    echo "Terminal 3: python3 main.py 3 --auto-mine"
    echo ""
}

# Function to run tests
run_tests() {
    echo "⏳ Waiting for nodes to start..."
    sleep 15
    
    echo "🧪 Running automated tests..."
    source "$VENV_DIR/bin/activate"
    python3 test_demo.py auto
}

# Function to show API examples
show_api_examples() {
    echo ""
    echo "🌐 API TESTING EXAMPLES"
    echo "======================="
    echo ""
    echo "1. Add a transaction:"
    echo "curl -X POST http://localhost:5001/transaction \\"
    echo "     -H 'Content-Type: application/json' \\"
    echo "     -d '{\"sender\":\"Alice\",\"recipient\":\"Bob\",\"amount\":50}'"
    echo ""
    echo "2. Check blockchain:"
    echo "curl http://localhost:5001/chain"
    echo ""
    echo "3. Get node info:"
    echo "curl http://localhost:5001/info"
    echo ""
    echo "4. Check balance:"
    echo "curl http://localhost:5001/balance/Alice"
    echo ""
    echo "5. Manual mining:"
    echo "curl http://localhost:5001/mine"
    echo ""
    echo "6. Force consensus:"
    echo "curl http://localhost:5001/consensus"
    echo ""
}

# Function to cleanup
cleanup() {
    echo ""
    echo "🧹 CLEANUP"
    echo "=========="
    echo "To remove the virtual environment:"
    echo "rm -rf $VENV_DIR"
    echo ""
}

# Check for virtual environment setup
check_venv() {
    if [ ! -d "$VENV_DIR" ]; then
        echo "⚠️  Virtual environment not found. Setting up..."
        setup_venv
    else
        echo "✅ Virtual environment found at $VENV_DIR"
    fi
}

# Main menu
echo ""
echo "Select demo mode:"
echo "0. Setup/Update virtual environment"
echo "1. Auto start 3 nodes with mining"
echo "2. Show manual instructions only"
echo "3. Run tests (nodes must be running)"
echo "4. Show API examples"
echo "5. Full demo (start nodes + run tests)"
echo "6. Cleanup virtual environment"

read -p "Enter choice (0-6): " choice

case $choice in
    0)
        setup_venv
        echo "✅ Environment setup complete!"
        ;;
    1)
        check_venv
        echo "🚀 Starting 3 Bitcoin nodes..."
        start_node 1 true
        start_node 2 true
        start_node 3 true
        show_manual_instructions
        show_api_examples
        echo ""
        echo "🎉 Nodes started! You can now run tests with option 3"
        ;;
    2)
        show_manual_instructions
        show_api_examples
        ;;
    3)
        check_venv
        run_tests
        ;;
    4)
        show_api_examples
        ;;
    5)
        check_venv
        echo "🎬 Full Demo Mode"
        echo "Starting nodes..."
        start_node 1 true
        start_node 2 true  
        start_node 3 true
        show_manual_instructions
        run_tests
        show_api_examples
        ;;
    6)
        cleanup
        ;;
    *)
        echo "❌ Invalid choice"
        exit 1
        ;;
esac

echo ""
echo "📚 Additional Notes:"
echo "- Each node runs on ports 5001, 5002, 5003"
echo "- Auto-mining is enabled (mines when 2+ transactions available)"  
echo "- Use Ctrl+C to stop nodes"
echo "- Check logs in each terminal for mining progress"
echo "- Virtual environment is located at: $VENV_DIR"
echo ""
echo "Happy testing! 🎉"