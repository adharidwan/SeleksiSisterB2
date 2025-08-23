# Simple HTTP Server v2.0

A lightweight HTTP server written in x86-64 Assembly language using FASM (Flat Assembler). This server implements core HTTP functionality with minimal but essential security features and supports multiple HTTP methods.

Made for Sister Selections

## Features

### HTTP Methods Supported
- **GET** - Retrieve files from the server
- **POST** - Upload data (saved to `post_data.txt`)
- **PUT** - Create or update specific files
- **DELETE** - Remove files from the server
- **HEAD** - Get headers without file content
- **OPTIONS** - Show supported methods and CORS headers

### Security Features
- **Path Traversal Protection** - Blocks `../` and absolute paths
- **Filename Validation** - Prevents access to system files
- **Safe Characters Only** - Filters dangerous characters in filenames

### Server Features
- **Multi-client Support** - Uses fork() for concurrent connections
- **Error Handling** - Proper HTTP status codes (200, 201, 400, 404, 405, 500)
- **Request Logging** - Shows incoming requests in console
- **Socket Reuse** - Prevents "address already in use" errors
- **CORS Support** - Handles preflight OPTIONS requests

## Requirements

- Linux x86-64 system
- FASM (Flat Assembler)
- Basic understanding of HTTP protocol

### Installing FASM

```bash
# Ubuntu/Debian
sudo apt-get install fasm

# Arch Linux
sudo pacman -S fasm

# Or download from: https://flatassembler.net/
```

## Compilation and Usage

### 1. Compile the Server
```bash
fasm main.asm
chmod +x main
```

### 2. Run the Server
```bash
./main
```

The server will display:
```
Simple HTTP Server v2.0
========================
Server listening on port 8080...
Press Ctrl+C to stop.
```

### 3. Stop the Server
Press `Ctrl+C` or kill the process:
```bash
pkill main
```

## API Documentation

### GET Requests

#### Get Default Page
```bash
curl http://localhost:8080/
# Returns index.html if it exists
```

#### Get Specific File
```bash
curl http://localhost:8080/filename.txt
# Returns the requested file
```

**Response Codes:**
- `200 OK` - File found and returned
- `404 Not Found` - File doesn't exist
- `400 Bad Request` - Invalid filename

### POST Requests

#### Upload Data
```bash
curl -X POST http://localhost:8080/ -d "Your data here"
# Saves data to post_data.txt
```

**Response:**
- `201 Created` - Data successfully saved

### PUT Requests

#### Create/Update File
```bash
curl -X PUT http://localhost:8080/myfile.txt -d "File content"
# Creates or overwrites myfile.txt
```

**Response Codes:**
- `200 OK` - File created/updated successfully
- `400 Bad Request` - Invalid filename

### DELETE Requests

#### Remove File
```bash
curl -X DELETE http://localhost:8080/myfile.txt
# Deletes myfile.txt
```

**Response Codes:**
- `200 OK` - File deleted successfully
- `404 Not Found` - File doesn't exist
- `400 Bad Request` - Invalid filename

### HEAD Requests

#### Get Headers Only
```bash
curl -I http://localhost:8080/index.html
# Returns headers without file content
```

### OPTIONS Requests

#### Check Supported Methods
```bash
curl -X OPTIONS http://localhost:8080/
# Returns allowed methods and CORS headers
```

## Testing Examples

### Basic File Serving
```bash
# Create test files
echo "<h1>Welcome!</h1>" > index.html
echo "<h1>Test Page</h1>" > test.html

# Test GET requests
curl http://localhost:8080/           # Returns index.html
curl http://localhost:8080/test.html  # Returns test.html
```

### File Upload and Management
```bash
# Upload via POST
curl -X POST http://localhost:8080/ -d "Hello from POST"
cat post_data.txt  # Check the uploaded data

# Create file via PUT
curl -X PUT http://localhost:8080/document.txt -d "Important document"
curl http://localhost:8080/document.txt  # Verify it was created

# Delete file
curl -X DELETE http://localhost:8080/document.txt
curl http://localhost:8080/document.txt  # Should return 404
```

### Security Testing
```bash
# These should all return 400 Bad Request
curl http://localhost:8080/../etc/passwd
curl http://localhost:8080/../../root/.bashrc
curl "http://localhost:8080/file..name"
```

### HTTP Compliance Testing
```bash
# Test different methods
curl -X GET http://localhost:8080/
curl -X HEAD http://localhost:8080/
curl -X OPTIONS http://localhost:8080/
curl -X PATCH http://localhost:8080/  # Should return 405

# Verbose output to see all headers
curl -v http://localhost:8080/
```

## Server Configuration

### Default Settings
- **Port:** 8080
- **Max Request Size:** 4096 bytes
- **Max File Size:** 16384 bytes
- **Connection Backlog:** 10
- **Default Index:** index.html

### Modifying Settings
Edit the constants in the source code:
```assembly
HTTP_PORT = 8080              ; Change server port
MAX_REQUEST_SIZE = 4096       ; Change max request size
MAX_FILE_SIZE = 16384         ; Change max file size
```

## File Structure

```
project/
├── main.asm              # Main server source code
├── main                  # Compiled executable
├── index.html            # Default page (optional)
├── uploaded.txt          # POST data storage
└── README.md             # This file
```

## HTTP Response Headers

The server includes proper HTTP headers:
```
HTTP/1.1 200 OK
Server: SimpleHTTP/2.0
Content-Type: text/html
Connection: close
```


## Architecture Overview

### Process Model
- **Parent Process**: Accepts connections and forks children
- **Child Process**: Handles individual client requests
- **Concurrent**: Multiple clients can connect simultaneously

### Security Model
- **Sandboxed**: Each request runs in isolated child process
- **Validated Input**: All filenames checked for safety
- **No Shell Access**: Pure system calls, no shell execution

### Memory Management
- **Stack-based**: Uses stack for local variables
- **Fixed Buffers**: Prevents buffer overflow attacks
- **No Dynamic Allocation**: Simple, predictable memory usage

## Performance Characteristics

- **Lightweight**: ~8KB executable size
- **Fast Startup**: Immediate server availability
- **Low Memory**: <1MB RSS per process
- **Concurrent**: Handles multiple clients via forking

## Limitations

- **No HTTPS**: Plain HTTP only
- **Basic Auth**: No authentication mechanism
- **Limited MIME**: Basic content-type detection
- **File Size**: Limited by buffer sizes
- **No Caching**: No cache headers or etags

## License

This project is provided as-is for labsister selection. Feel free to modify and distribute.


