format ELF64 executable 3

entry start

; const
; Socket constants
AF_INET      = 2
SOCK_STREAM = 1
SOL_SOCKET  = 1
SO_REUSEADDR = 2

; File constants
O_RDONLY    = 0
O_WRONLY    = 1
O_RDWR      = 2
O_CREAT     = 64
O_TRUNC     = 512
O_APPEND    = 1024

; System calls
SYS_READ       = 0
SYS_WRITE      = 1
SYS_OPEN       = 2
SYS_CLOSE      = 3
SYS_STAT       = 4
SYS_LSEEK      = 8
SYS_SOCKET     = 41
SYS_ACCEPT     = 43
SYS_BIND       = 49
SYS_LISTEN     = 50
SYS_SETSOCKOPT = 54
SYS_FORK       = 57
SYS_EXIT       = 60
SYS_UNLINK     = 87

; HTTP constants
MAX_REQUEST_SIZE = 4096
MAX_FILE_SIZE = 16384
MAX_FILENAME_LEN = 255
HTTP_PORT = 8080

; code
segment readable executable

start:
  pop rax             
  mov rsi, [rsp]      
  
  cmp rax, 2          
  jl show_usage       
  mov rsi, [rsp+8]    
  call string_to_uint 
  
  cmp rax, 0          
  je invalid_port
  cmp rax, 65535      
  jg invalid_port

  mov [server_port], ax 
  call print_startup_banner
  call create_and_bind_socket
  call start_server_loop

show_usage:
  mov rax, SYS_WRITE
  mov rdi, 2 ; stderr
  mov rsi, usage_msg
  mov rdx, len_usage_msg
  syscall
  jmp exit_error

invalid_port:
  mov rax, SYS_WRITE
  mov rdi, 2 ; stderr
  mov rsi, error_port
  mov rdx, len_error_port
  syscall
  jmp exit_error

create_and_bind_socket:
  ; Create socket
  mov rax, SYS_SOCKET
  mov rdi, AF_INET
  mov rsi, SOCK_STREAM
  xor rdx, rdx
  syscall

  cmp rax, 0
  jl socket_error
  mov [server_socket], rax

  mov rax, SYS_SETSOCKOPT
  mov rdi, [server_socket]
  mov rsi, SOL_SOCKET
  mov rdx, SO_REUSEADDR
  mov r10, socket_option_one
  mov r8, 4
  syscall

  mov word [sockaddr_in], AF_INET
  mov ax, [server_port]       
  rol ax, 8                   
  mov word [sockaddr_in+2], ax    
  mov dword [sockaddr_in+4], 0     

  ; Bind socket
  mov rax, SYS_BIND
  mov rdi, [server_socket]
  mov rsi, sockaddr_in
  mov rdx, 16
  syscall

  cmp rax, 0
  jl bind_error

  ; Listen for connections
  mov rax, SYS_LISTEN
  mov rdi, [server_socket]
  mov rsi, 10 ; Increased backlog
  syscall

  cmp rax, 0
  jl listen_error

  call print_server_ready
  ret

start_server_loop:
accept_loop:
  ; Accept a connection
  mov rax, SYS_ACCEPT
  mov rdi, [server_socket]
  xor rsi, rsi
  xor rdx, rdx
  syscall
  
  cmp rax, 0
  jl accept_loop ; Ignore failed accepts
  mov [client_socket], rax
  
  ; Fork to handle the new client
  mov rax, SYS_FORK
  syscall
  
  cmp rax, 0
  je  child_process
  jg  parent_process
  
  ; Fork failed, close client socket and continue
  mov rax, SYS_CLOSE
  mov rdi, [client_socket]
  syscall
  jmp accept_loop

parent_process:
  ; Parent closes client socket and loops back
  mov rax, SYS_CLOSE
  mov rdi, [client_socket]
  syscall
  jmp accept_loop

child_process:
  ; Child closes listening socket
  mov rax, SYS_CLOSE
  mov rdi, [server_socket]
  syscall
  
  call handle_client_request
  
  ; Close client socket and exit child
  mov rax, SYS_CLOSE
  mov rdi, [client_socket]
  syscall
  
  mov rax, SYS_EXIT
  xor rdi, rdi
  syscall

; --- Client Request Handler ---
handle_client_request:
  ; Clear request buffer
  mov rdi, request_buffer
  mov rcx, MAX_REQUEST_SIZE
  xor al, al
  rep stosb
  
  ; Read request from client
  mov rax, SYS_READ
  mov rdi, [client_socket]
  mov rsi, request_buffer
  mov rdx, MAX_REQUEST_SIZE - 1
  syscall
  
  cmp rax, 0
  jle invalid_request
  mov [request_length], rax
  
  ; Log the request
  call log_request
  
  ; Route by HTTP method
  mov eax, dword [request_buffer]
  cmp eax, 'GET '
  je handle_get
  cmp eax, 'POST'
  je handle_post
  cmp eax, 'PUT '
  je handle_put
  cmp eax, 'DELE'
  je handle_delete
  cmp eax, 'HEAD'
  je handle_head
  cmp eax, 'OPTI'
  je handle_options
  
  jmp send_405_response

invalid_request:
  jmp send_400_response

; --- HTTP Method Handlers ---
handle_get:
  lea rsi, [request_buffer + 4] ; Skip "GET "
  call parse_request_path
  
  cmp byte [parsed_filename], 0
  jne .validate_filename
  
  ; Default to index.html
  mov rsi, default_index
  mov rdi, parsed_filename
  call copy_string
  
.validate_filename:
  mov rdi, parsed_filename
  call is_filename_safe
  cmp rax, 1
  jne send_400_response
  
  call serve_file
  ret

handle_head:
  ; Same as GET but only send headers
  lea rsi, [request_buffer + 5] ; Skip "HEAD "
  call parse_request_path
  
  cmp byte [parsed_filename], 0
  jne .validate_filename
  
  mov rsi, default_index
  mov rdi, parsed_filename
  call copy_string
  
.validate_filename:
  mov rdi, parsed_filename
  call is_filename_safe
  cmp rax, 1
  jne send_400_response
  
  call send_file_headers_only
  ret

handle_options:
  call send_options_response
  ret

handle_post:
  call find_request_body
  cmp rax, 0
  je send_400_response
  
  mov rsi, post_default_filename
  mov rdi, parsed_filename
  call copy_string
  
  call write_body_to_file
  call send_201_response
  ret

handle_put:
  lea rsi, [request_buffer + 4] ; Skip "PUT "
  call parse_request_path
  
  mov rdi, parsed_filename
  call is_filename_safe
  cmp rax, 1
  jne send_400_response
  
  call find_request_body
  cmp rax, 0
  je send_400_response
  
  call write_body_to_file
  call send_200_response
  ret

handle_delete:
  lea rsi, [request_buffer + 7] ; Skip "DELETE "
  call parse_request_path
  
  mov rdi, parsed_filename
  call is_filename_safe
  cmp rax, 1
  jne send_400_response
  
  ; Check if file exists before attempting delete
  call file_exists
  cmp rax, 0
  je send_404_response
  
  mov rax, SYS_UNLINK
  mov rdi, parsed_filename
  syscall
  
  cmp rax, 0
  jne send_500_response
  
  call send_200_response
  ret

; --- Utility Functions ---
;; MOD: New function to convert string to unsigned integer (atoi)
;; Input: rsi = pointer to null-terminated string
;; Output: rax = converted integer, or 0 on error
string_to_uint:
  xor rax, rax      ; Clear result register
  xor rcx, rcx      ; Clear temp register for character
.loop:
  mov cl, byte [rsi]
  cmp cl, 0
  je .done          ; If null terminator, we're done

  cmp cl, '0'
  jl .error         ; If char is less than '0', it's not a digit
  cmp cl, '9'
  jg .error         ; If char is greater than '9', it's not a digit
  
  sub cl, '0'       ; Convert ASCII digit to integer value
  
  imul rax, rax, 10 ; Multiply current result by 10
  add rax, rcx      ; Add the new digit
  
  inc rsi
  jmp .loop
.done:
  ret
.error:
  xor rax, rax      ; Return 0 on error
  ret

parse_request_path:
  mov rdi, parsed_filename
  
  ; Skip leading '/' if present
  cmp byte [rsi], '/'
  jne .parse_loop
  inc rsi
  
.parse_loop:
  mov al, byte [rsi]
  cmp al, ' '
  je .end_path
  cmp al, '?'
  je .end_path
  cmp al, 0
  je .end_path
  
  mov byte [rdi], al
  inc rsi
  inc rdi
  jmp .parse_loop
  
.end_path:
  mov byte [rdi], 0
  ret

find_request_body:
  xor rcx, rcx
  mov rdx, [request_length]
  sub rdx, 3 ; Need at least 4 bytes for CRLFCRLF
  
.search_loop:
  cmp rcx, rdx
  jge .not_found
  
  mov eax, dword [request_buffer + rcx]
  cmp eax, 0x0A0D0A0D ; CRLFCRLF
  je .found_body
  
  inc rcx
  jmp .search_loop
  
.found_body:
  lea rax, [request_buffer + rcx + 4]
  mov [request_body_ptr], rax
  
  mov rdx, [request_length]
  sub rdx, rcx
  sub rdx, 4
  mov [request_body_length], rdx
  
  mov rax, 1
  ret
  
.not_found:
  xor rax, rax
  ret

write_body_to_file:
  mov rax, SYS_OPEN
  mov rdi, parsed_filename
  mov rsi, O_WRONLY or O_CREAT or O_TRUNC
  mov rdx, 644o
  syscall
  
  cmp rax, 0
  jl .write_error
  mov [file_descriptor], rax
  
  mov rax, SYS_WRITE
  mov rdi, [file_descriptor]
  mov rsi, [request_body_ptr]
  mov rdx, [request_body_length]
  syscall
  
  mov rax, SYS_CLOSE
  mov rdi, [file_descriptor]
  syscall
  ret
  
.write_error:
  ret

serve_file:
  mov rax, SYS_OPEN
  mov rdi, parsed_filename
  mov rsi, O_RDONLY
  xor rdx, rdx
  syscall
  
  cmp rax, 0
  jl send_404_response
  mov [file_descriptor], rax
  
  ; Get file size using lseek
  mov rax, SYS_LSEEK
  mov rdi, [file_descriptor]
  xor rsi, rsi
  mov rdx, 2 ; SEEK_END
  syscall
  mov [file_size], rax
  
  ; Reset to beginning
  mov rax, SYS_LSEEK
  mov rdi, [file_descriptor]
  xor rsi, rsi
  xor rdx, rdx ; SEEK_SET
  syscall
  
  ; Read file content
  mov rax, SYS_READ
  mov rdi, [file_descriptor]
  mov rsi, file_buffer
  mov rdx, MAX_FILE_SIZE
  syscall
  mov [bytes_read], rax
  
  mov rax, SYS_CLOSE
  mov rdi, [file_descriptor]
  syscall
  
  ; Send HTTP response
  call send_file_with_headers
  ret

send_file_headers_only:
  mov rax, SYS_OPEN
  mov rdi, parsed_filename
  mov rsi, O_RDONLY
  xor rdx, rdx
  syscall
  
  cmp rax, 0
  jl send_404_response
  mov [file_descriptor], rax
  
  ; Get file size
  mov rax, SYS_LSEEK
  mov rdi, [file_descriptor]
  xor rsi, rsi
  mov rdx, 2 ; SEEK_END
  syscall
  mov [file_size], rax
  
  mov rax, SYS_CLOSE
  mov rdi, [file_descriptor]
  syscall
  
  ; Send only headers
  call send_200_headers
  ret

send_file_with_headers:
  call send_200_headers
  
  ; Send file content
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, file_buffer
  mov rdx, [bytes_read]
  syscall
  ret

; --- Security Function ---
is_filename_safe:
  ; Check for empty string
  cmp byte [rdi], 0
  je .unsafe
  
.check_loop:
  mov al, byte [rdi]
  cmp al, 0
  je .safe
  
  ; Disallow path separators
  cmp al, '/'
  je .unsafe
  cmp al, '\'
  je .unsafe
  
  ; Check for dot sequences
  cmp al, '.'
  jne .continue_loop
  
  ; Check for .. (parent directory)
  cmp byte [rdi + 1], '.'
  je .unsafe
  
.continue_loop:
  inc rdi
  jmp .check_loop
  
.safe:
  mov rax, 1
  ret
  
.unsafe:
  xor rax, rax
  ret

file_exists:
  mov rax, SYS_OPEN
  mov rdi, parsed_filename
  mov rsi, O_RDONLY
  xor rdx, rdx
  syscall
  
  cmp rax, 0
  jl .not_exists
  
  ; File exists, close it
  mov rdi, rax
  mov rax, SYS_CLOSE
  syscall
  
  mov rax, 1
  ret
  
.not_exists:
  xor rax, rax
  ret

copy_string:
  ; rsi = source, rdi = destination
.copy_loop:
  mov al, byte [rsi]
  mov byte [rdi], al
  cmp al, 0
  je .done
  inc rsi
  inc rdi
  jmp .copy_loop
.done:
  ret

; --- HTTP Response Functions ---
send_200_headers:
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, http_200_headers
  mov rdx, len_200_headers
  syscall
  ret

send_200_response:
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, http_200_response
  mov rdx, len_200_response
  syscall
  ret

send_201_response:
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, http_201_response
  mov rdx, len_201_response
  syscall
  ret

send_400_response:
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, http_400_response
  mov rdx, len_400_response
  syscall
  ret

send_404_response:
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, http_404_response
  mov rdx, len_404_response
  syscall
  ret

send_405_response:
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, http_405_response
  mov rdx, len_405_response
  syscall
  ret

send_500_response:
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, http_500_response
  mov rdx, len_500_response
  syscall
  ret

send_options_response:
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, http_options_response
  mov rdx, len_options_response
  syscall
  ret

; --- Logging and Output Functions ---
print_startup_banner:
  mov rax, SYS_WRITE
  mov rdi, 1 ; stdout
  mov rsi, startup_banner
  mov rdx, len_startup_banner
  syscall
  ret

print_server_ready:
  mov rax, SYS_WRITE
  mov rdi, 1 ; stdout
  mov rsi, server_ready_msg
  mov rdx, len_server_ready_msg
  syscall
  ret

log_request:
  mov rax, SYS_WRITE
  mov rdi, 1 ; stdout
  mov rsi, log_prefix
  mov rdx, len_log_prefix
  syscall
  
  ; Extract and print method and path
  mov rsi, request_buffer
  mov rcx, 0
.find_newline:
  cmp byte [rsi + rcx], 0x0D
  je .found_newline
  cmp byte [rsi + rcx], 0x0A
  je .found_newline
  inc rcx
  cmp rcx, 100 ; Limit to 100 chars
  jl .find_newline
.found_newline:
  
  mov rax, SYS_WRITE
  mov rdi, 1 ; stdout
  mov rsi, request_buffer
  mov rdx, rcx
  syscall
  
  mov rax, SYS_WRITE
  mov rdi, 1 ; stdout
  mov rsi, newline
  mov rdx, 1
  syscall
  ret

; --- Error Handlers ---
socket_error:
  mov rax, SYS_WRITE
  mov rdi, 2 ; stderr
  mov rsi, error_socket
  mov rdx, len_error_socket
  syscall
  jmp exit_error

bind_error:
  mov rax, SYS_WRITE
  mov rdi, 2 ; stderr
  mov rsi, error_bind
  mov rdx, len_error_bind
  syscall
  jmp exit_error

listen_error:
  mov rax, SYS_WRITE
  mov rdi, 2 ; stderr
  mov rsi, error_listen
  mov rdx, len_error_listen
  syscall
  jmp exit_error

exit_error:
  mov rax, SYS_EXIT
  mov rdi, 1
  syscall

;data
segment readable writeable

; --- Messages ---
startup_banner db 'Simple HTTP Server v2.0', 0x0A, '========================', 0x0A
len_startup_banner = $ - startup_banner

;; MOD: Changed the server ready message to be more generic
server_ready_msg db 'Server listening on the configured port...', 0x0A, 'Press Ctrl+C to stop.', 0x0A
len_server_ready_msg = $ - server_ready_msg

log_prefix db '[REQUEST] '
len_log_prefix = $ - log_prefix

newline db 0x0A

;; MOD: New messages for argument handling
usage_msg db 'Usage: ./server <port>', 0x0A
len_usage_msg = $ - usage_msg
error_port db 'ERROR: Invalid port. Must be a number between 1 and 65535.', 0x0A
len_error_port = $ - error_port

; --- Error Messages ---
error_socket db 'ERROR: Failed to create socket', 0x0A
len_error_socket = $ - error_socket

error_bind db 'ERROR: Failed to bind socket (port may be in use)', 0x0A
len_error_bind = $ - error_bind

error_listen db 'ERROR: Failed to listen on socket', 0x0A
len_error_listen = $ - error_listen

; --- HTTP Responses ---
http_200_headers db 'HTTP/1.1 200 OK', 0x0D, 0x0A
                  db 'Server: SimpleHTTP/2.0', 0x0D, 0x0A
                  db 'Content-Type: text/html', 0x0D, 0x0A
                  db 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
len_200_headers = $ - http_200_headers

http_200_response db 'HTTP/1.1 200 OK', 0x0D, 0x0A
                  db 'Server: SimpleHTTP/2.0', 0x0D, 0x0A
                  db 'Content-Type: text/plain', 0x0D, 0x0A
                  db 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
                  db 'Operation completed successfully.', 0x0A
len_200_response = $ - http_200_response

http_201_response db 'HTTP/1.1 201 Created', 0x0D, 0x0A
                  db 'Server: SimpleHTTP/2.0', 0x0D, 0x0A
                  db 'Content-Type: text/plain', 0x0D, 0x0A
                  db 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
                  db 'Resource created successfully.', 0x0A
len_201_response = $ - http_201_response

http_400_response db 'HTTP/1.1 400 Bad Request', 0x0D, 0x0A
                  db 'Server: SimpleHTTP/2.0', 0x0D, 0x0A
                  db 'Content-Type: text/html', 0x0D, 0x0A
                  db 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
                  db '<h1>400 Bad Request</h1><p>Invalid request format or unsafe filename.</p>', 0x0A
len_400_response = $ - http_400_response

http_404_response db 'HTTP/1.1 404 Not Found', 0x0D, 0x0A
                  db 'Server: SimpleHTTP/2.0', 0x0D, 0x0A
                  db 'Content-Type: text/html', 0x0D, 0x0A
                  db 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
                  db '<h1>404 Not Found</h1><p>The requested resource was not found.</p>', 0x0A
len_404_response = $ - http_404_response

http_405_response db 'HTTP/1.1 405 Method Not Allowed', 0x0D, 0x0A
                  db 'Server: SimpleHTTP/2.0', 0x0D, 0x0A
                  db 'Allow: GET, POST, PUT, DELETE, HEAD, OPTIONS', 0x0D, 0x0A
                  db 'Content-Type: text/html', 0x0D, 0x0A
                  db 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
                  db '<h1>405 Method Not Allowed</h1><p>Supported methods: GET, POST, PUT, DELETE, HEAD, OPTIONS</p>', 0x0A
len_405_response = $ - http_405_response

http_500_response db 'HTTP/1.1 500 Internal Server Error', 0x0D, 0x0A
                  db 'Server: SimpleHTTP/2.0', 0x0D, 0x0A
                  db 'Content-Type: text/html', 0x0D, 0x0A
                  db 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
                  db '<h1>500 Internal Server Error</h1><p>An error occurred while processing your request.</p>', 0x0A
len_500_response = $ - http_500_response

http_options_response db 'HTTP/1.1 200 OK', 0x0D, 0x0A
                      db 'Server: SimpleHTTP/2.0', 0x0D, 0x0A
                      db 'Allow: GET, POST, PUT, DELETE, HEAD, OPTIONS', 0x0D, 0x0A
                      db 'Access-Control-Allow-Origin: *', 0x0D, 0x0A
                      db 'Access-Control-Allow-Methods: GET, POST, PUT, DELETE, HEAD, OPTIONS', 0x0D, 0x0A
                      db 'Access-Control-Allow-Headers: Content-Type', 0x0D, 0x0A
                      db 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
len_options_response = $ - http_options_response

; --- Default Files ---
default_index db 'index.html', 0
post_default_filename db 'post_data.txt', 0

; --- Socket option ---
socket_option_one dd 1

; --- Buffers and Variables ---
sockaddr_in           rb 16
request_buffer        rb MAX_REQUEST_SIZE
file_buffer           rb MAX_FILE_SIZE
parsed_filename       rb MAX_FILENAME_LEN

server_socket         rq 1
client_socket         rq 1
file_descriptor       rq 1
request_length        rq 1
request_body_ptr      rq 1
request_body_length   rq 1
file_size             rq 1
bytes_read            rq 1
server_port           dw 0