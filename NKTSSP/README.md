# NKTSSP - Simple HTTP Server v2.0

A lightweight HTTP server written in x86-64 Assembly language using NASM with GCC compatibility. This server implements core HTTP functionality with authentication-based route protection and supports multiple HTTP methods.

Made for Sister Selections

## Features

### HTTP Methods Supported
- **GET** - Retrieve files from the server
- **POST** - Upload data (saved to `post_data.txt`)
- **PUT** - Create or update specific files
- **DELETE** - Remove files from the server
- **HEAD** - Get headers without file content
- **OPTIONS** - Show supported methods and CORS headers

### Authentication & Security Features
- **Selective Authentication** - Only specific routes (like `secret.html`) require authentication
- **Bearer Token Authentication** - Uses `Authorization: Bearer <token>` header
- **Path Traversal Protection** - Blocks `../` and absolute paths
- **Filename Validation** - Prevents access to system files
- **Safe Characters Only** - Filters dangerous characters in filenames

### Server Features
- **Multi-client Support** - Uses fork() for concurrent connections
- **Error Handling** - Proper HTTP status codes (200, 201, 400, 401, 404, 405, 500)
- **Request Logging** - Shows incoming requests in console
- **Socket Reuse** - Prevents "address already in use" errors
- **CORS Support** - Handles preflight OPTIONS requests
- **Docker Support** - Containerized deployment with Dockerfile
- **Cloud Deployment Ready** - Includes Azure deployment guide

## Requirements

### Local Development
- Linux x86-64 system
- NASM (Netwide Assembler)
- GCC compiler
- Make utility

### Docker Deployment
- Docker Engine
- Docker Compose (optional)

### Cloud Deployment
- Azure CLI
- Domain name (for HTTPS setup)

## Installation & Setup

### Installing Dependencies (Ubuntu/Debian)
```bash
sudo apt-get update
sudo apt-get install -y build-essential make nasm
```

### Installing Dependencies (Arch Linux)
```bash
sudo pacman -S base-devel nasm
```

## Compilation and Usage

### Method 1: Direct Compilation
```bash
# Compile using the provided Makefile
make

# Run the server on default port 8080
./main 8080
```

### Method 2: Docker Deployment
```bash
# Build Docker image
docker build -t nktssp-server .

# Run container
docker run -d -p 8080:8080 --name nktssp-server nktssp-server

# View logs
docker logs nktssp-server

# Stop container
docker stop nktssp-server
```

### Method 3: Cloud Deployment (Azure)
Follow the detailed guide in `deploy.md` for Azure deployment with HTTPS support.

## Server Configuration

The server will display:
```
Hutao HTTP Server v2.0
========================
Server listening on the configured port...
Press Ctrl+C to stop.
```

### Default Settings
- **Port:** Configurable via command line argument
- **Max Request Size:** 4096 bytes
- **Max File Size:** 16384 bytes
- **Connection Backlog:** 10
- **Default Index:** index.html
- **Authentication Token:** `HUTAOTHEGOAT` (defined in auth.c)

## API Documentation

### Authentication
Certain routes require authentication using a Bearer token in the Authorization header:

```bash
# Access protected route (secret.html)
curl -H "Authorization: Bearer HUTAOTHEGOAT" http://localhost:8080/secret.html
```

**Protected Routes:**
- `secret.html` - Requires valid Bearer token

**Public Routes:**
- All other routes are publicly accessible

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

#### Access Protected Content
```bash
curl -H "Authorization: Bearer HUTAOTHEGOAT" http://localhost:8080/secret.html
# Returns secret content with valid token
```

**Response Codes:**
- `200 OK` - File found and returned
- `401 Unauthorized` - Invalid or missing token for protected routes
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

### Authentication Testing
```bash
# Try accessing protected route without token (should return 401)
curl http://localhost:8080/secret.html

# Access with correct token (should return 200)
curl -H "Authorization: Bearer HUTAOTHEGOAT" http://localhost:8080/secret.html

# Try with wrong token (should return 401)
curl -H "Authorization: Bearer WRONGTOKEN" http://localhost:8080/secret.html
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

## Project Structure

```
NKTSSP/
├── main.asm              # Main server source code (Assembly)
├── auth.c                # Authentication logic (C)
├── Makefile              # Build configuration
├── Dockerfile            # Docker container configuration
├── deploy.md             # Azure deployment guide
├── index.html            # Default homepage
├── secret.html           # Protected content (requires auth)
├── post_data.txt         # POST data storage
├── uploaded.txt          # Example uploaded file
└── README.md             # This file
```

## HTTP Response Headers

The server includes proper HTTP headers:
```
HTTP/1.1 200 OK
Server: HutaoHTTP/2.0
Content-Type: text/html
Connection: close
```

For protected routes without proper authentication:
```
HTTP/1.1 401 Unauthorized
Server: HutaoHTTP/2.0
Content-Type: text/html
Connection: close
```

## Architecture Overview

### Hybrid Language Design
- **Assembly Core** - High-performance server implementation in x86-64 assembly
- **C Authentication** - Authentication logic implemented in C for maintainability
- **GCC Linking** - Seamless integration between Assembly and C components

### Process Model
- **Parent Process** - Accepts connections and forks children
- **Child Process** - Handles individual client requests with authentication checks
- **Concurrent** - Multiple clients can connect simultaneously

### Security Model
- **Selective Protection** - Only specified routes require authentication
- **Token-based Auth** - Simple Bearer token authentication
- **Sandboxed Execution** - Each request runs in isolated child process
- **Input Validation** - All filenames and paths checked for safety
- **No Shell Access** - Pure system calls, no shell execution

### Memory Management
- **Stack-based** - Uses stack for local variables
- **Fixed Buffers** - Prevents buffer overflow attacks
- **No Dynamic Allocation** - Simple, predictable memory usage

## Performance Characteristics

- **Lightweight** - ~8KB executable size
- **Fast Startup** - Immediate server availability
- **Low Memory** - <1MB RSS per process
- **Concurrent** - Handles multiple clients via forking
- **Efficient Auth** - Authentication only checked for protected routes

## Deployment Options

### 1. Local Development
- Direct compilation and execution
- Suitable for development and testing

### 2. Docker Container
- Containerized deployment
- Consistent environment across platforms
- Easy scaling and management

### 3. Cloud Deployment
- Azure VM with Docker
- HTTPS support with Let's Encrypt
- Custom domain configuration
- Production-ready setup

See `deploy.md` for detailed cloud deployment instructions.

## Configuration

### Modifying Authentication Token
Edit the `SECRET_TOKEN` in `auth.c`:
```c
#define SECRET_TOKEN "YOURNEWTOKEN"
```

### Adding Protected Routes
Modify the `is_protected_route()` function in `auth.c`:
```c
int is_protected_route(const char *filename) {
    if (filename != NULL && (
        strcmp(filename, "secret.html") == 0 ||
        strcmp(filename, "admin.html") == 0
    )) {
        return 1;
    }
    return 0;
}
```

### Modifying Server Settings
Edit the constants in `main.asm`:
```assembly
; Request/Response buffer sizes
request_buffer        resb 4096      ; Max request size
file_buffer           resb 16384     ; Max file size
```

## Limitations

- **No HTTPS** - Plain HTTP only (use reverse proxy for HTTPS)
- **Simple Auth** - Basic Bearer token authentication
- **Limited MIME** - Basic content-type detection
- **File Size** - Limited by buffer sizes
- **No Caching** - No cache headers or etags
- **No Session Management** - Stateless authentication only

## Troubleshooting

### Port Already in Use
```bash
# Find process using the port
sudo lsof -i :8080

# Kill the process
sudo kill -9 <PID>
```

### Permission Denied
```bash
# Make executable
chmod +x main

# Check if port requires sudo (ports < 1024)
sudo ./main 80
```

### Docker Issues
```bash
# View container logs
docker logs nktssp-server

# Debug inside container
docker exec -it nktssp-server /bin/bash
```

## Live Demo

The server is currently deployed and accessible at: `hutaogaot.space`

Example requests:
- `https://hutaogaot.space/` - Public homepage
- `https://hutaogaot.space/secret.html` - Protected content (requires token)

## License

This project is provided as-is for educational purposes. Feel free to modify and distribute.