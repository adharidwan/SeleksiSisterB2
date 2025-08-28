extern check_authentication
extern is_protected_route

section .data
AF_INET      equ 2
SOCK_STREAM equ 1
SOL_SOCKET  equ 1
SO_REUSEADDR equ 2
O_RDONLY    equ 0
O_WRONLY    equ 1
O_RDWR      equ 2
O_CREAT     equ 64
O_TRUNC     equ 512
O_APPEND    equ 1024
SYS_READ       equ 0
SYS_WRITE      equ 1
SYS_OPEN       equ 2
SYS_CLOSE      equ 3
SYS_LSEEK      equ 8
SYS_SOCKET     equ 41
SYS_ACCEPT     equ 43
SYS_BIND       equ 49
SYS_LISTEN     equ 50
SYS_SETSOCKOPT equ 54
SYS_FORK       equ 57
SYS_EXIT       equ 60
SYS_UNLINK     equ 87

section .data
startup_banner db 'Hutao HTTP Server v2.0', 0x0A, '========================', 0x0A
len_startup_banner equ $ - startup_banner
server_ready_msg db 'Server listening on the configured port...', 0x0A, 'Press Ctrl+C to stop.', 0x0A
len_server_ready_msg equ $ - server_ready_msg
log_prefix db '[REQUEST] '
len_log_prefix equ $ - log_prefix
newline db 0x0A
usage_msg db 'Usage: ./nktssrv <port>', 0x0A
len_usage_msg equ $ - usage_msg
error_port db 'ERROR: Invalid port. Must be a number between 1 and 65535.', 0x0A
len_error_port equ $ - error_port
error_socket db 'ERROR: Failed to create socket', 0x0A
len_error_socket equ $ - error_socket
error_bind db 'ERROR: Failed to bind socket (port may be in use)', 0x0A
len_error_bind equ $ - error_bind
error_listen db 'ERROR: Failed to listen on socket', 0x0A
len_error_listen equ $ - error_listen
http_200_headers db 'HTTP/1.1 200 OK', 0x0D, 0x0A, 'Server: HutaoHTTP/2.0', 0x0D, 0x0A, 'Content-Type: text/html', 0x0D, 0x0A, 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
len_200_headers equ $ - http_200_headers
http_200_response db 'HTTP/1.1 200 OK', 0x0D, 0x0A, 'Server: HutaoHTTP/2.0', 0x0D, 0x0A, 'Content-Type: text/plain', 0x0D, 0x0A, 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A, 'Operation completed successfully.', 0x0A
len_200_response equ $ - http_200_response
http_201_response db 'HTTP/1.1 201 Created', 0x0D, 0x0A, 'Server: HutaoHTTP/2.0', 0x0D, 0x0A, 'Content-Type: text/plain', 0x0D, 0x0A, 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A, 'Resource created successfully.', 0x0A
len_201_response equ $ - http_201_response
http_401_response db 'HTTP/1.1 401 Unauthorized', 0x0D, 0x0A, 'Server: HutaoHTTP/2.0', 0x0D, 0x0A, 'Content-Type: text/html', 0x0D, 0x0A, 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A, '<h1>401 Unauthorized</h1><p>A valid token is required.</p>', 0x0A
len_401_response equ $ - http_401_response
http_400_response db 'HTTP/1.1 400 Bad Request', 0x0D, 0x0A, 'Server: HutaoHTTP/2.0', 0x0D, 0x0A, 'Content-Type: text/html', 0x0D, 0x0A, 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A, '<h1>400 Bad Request</h1><p>Invalid request format or unsafe filename.</p>', 0x0A
len_400_response equ $ - http_400_response
http_404_response db 'HTTP/1.1 404 Not Found', 0x0D, 0x0A, 'Server: HutaoHTTP/2.0', 0x0D, 0x0A, 'Content-Type: text/html', 0x0D, 0x0A, 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A, '<h1>404 Not Found</h1><p>The requested resource was not found.</p>', 0x0A
len_404_response equ $ - http_404_response
http_405_response db 'HTTP/1.1 405 Method Not Allowed', 0x0D, 0x0A, 'Server: HutaoHTTP/2.0', 0x0D, 0x0A, 'Allow: GET, POST, PUT, DELETE, HEAD, OPTIONS', 0x0D, 0x0A, 'Content-Type: text/html', 0x0D, 0x0A, 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A, '<h1>405 Method Not Allowed</h1><p>Supported methods: GET, POST, PUT, DELETE, HEAD, OPTIONS</p>', 0x0A
len_405_response equ $ - http_405_response
http_500_response db 'HTTP/1.1 500 Internal Server Error', 0x0D, 0x0A, 'Server: HutaoHTTP/2.0', 0x0D, 0x0A, 'Content-Type: text/html', 0x0D, 0x0A, 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A, '<h1>500 Internal Server Error</h1><p>An error occurred while processing your request.</p>', 0x0A
len_500_response equ $ - http_500_response
http_options_response db 'HTTP/1.1 200 OK', 0x0D, 0x0A, 'Server: HutaoHTTP/2.0', 0x0D, 0x0A, 'Allow: GET, POST, PUT, DELETE, HEAD, OPTIONS', 0x0D, 0x0A, 'Access-Control-Allow-Origin: *', 0x0D, 0x0A, 'Access-Control-Allow-Methods: GET, POST, PUT, DELETE, HEAD, OPTIONS', 0x0D, 0x0A, 'Access-Control-Allow-Headers: Content-Type', 0x0D, 0x0A, 'Connection: close', 0x0D, 0x0A, 0x0D, 0x0A
len_options_response equ $ - http_options_response
default_index db 'index.html', 0
post_default_filename db 'post_data.txt', 0
socket_option_one dd 1

section .bss
sockaddr_in           resb 16
request_buffer        resb 4096
file_buffer           resb 16384
parsed_filename       resb 255
server_socket         resq 1
client_socket         resq 1
file_descriptor       resq 1
request_length        resq 1
request_body_ptr      resq 1
request_body_length   resq 1
file_size             resq 1
bytes_read            resq 1
server_port           resw 1

section .text
global main

main:
  cmp rdi, 2
  jl show_usage
  mov rax, [rsi + 8]
  mov rsi, rax
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
  mov rdi, 2
  mov rsi, usage_msg
  mov rdx, len_usage_msg
  syscall
  jmp exit_error

invalid_port:
  mov rax, SYS_WRITE
  mov rdi, 2
  mov rsi, error_port
  mov rdx, len_error_port
  syscall
  jmp exit_error

create_and_bind_socket:
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
  mov rax, SYS_BIND
  mov rdi, [server_socket]
  mov rsi, sockaddr_in
  mov rdx, 16
  syscall
  cmp rax, 0
  jl bind_error
  mov rax, SYS_LISTEN
  mov rdi, [server_socket]
  mov rsi, 10
  syscall
  cmp rax, 0
  jl listen_error
  call print_server_ready
  ret

start_server_loop:
accept_loop:
  mov rax, SYS_ACCEPT
  mov rdi, [server_socket]
  xor rsi, rsi
  xor rdx, rdx
  syscall
  cmp rax, 0
  jl accept_loop
  mov [client_socket], rax
  mov rax, SYS_FORK
  syscall
  cmp rax, 0
  je  child_process
  jg  parent_process
  mov rax, SYS_CLOSE
  mov rdi, [client_socket]
  syscall
  jmp accept_loop

parent_process:
  mov rax, SYS_CLOSE
  mov rdi, [client_socket]
  syscall
  jmp accept_loop

child_process:
  mov rax, SYS_CLOSE
  mov rdi, [server_socket]
  syscall
  call handle_client_request
  mov rax, SYS_CLOSE
  mov rdi, [client_socket]
  syscall
  mov rax, SYS_EXIT
  xor rdi, rdi
  syscall

handle_client_request:
  mov rdi, request_buffer
  mov rcx, 4096
  xor al, al
  rep stosb
  
  mov rax, SYS_READ
  mov rdi, [client_socket]
  mov rsi, request_buffer
  mov rdx, 4095
  syscall
  
  cmp rax, 0
  jle invalid_request
  mov [request_length], rax
  
  lea rsi, [request_buffer + 4] ; Asumsi GET, lewati "GET "
  call parse_request_path

  mov rdi, parsed_filename
  call is_protected_route
  
  cmp rax, 1 ; Apakah hasilnya 1 (true)?
  jne .skip_auth ; Jika tidak, lewati pemeriksaan otentikasi

  mov rdi, request_buffer
  call check_authentication
  cmp rax, 0 
  je send_401_response

.skip_auth:
  call log_request
  
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

handle_get:
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
  call serve_file
  ret

handle_head:
  lea rsi, [request_buffer + 5]
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
  lea rsi, [request_buffer + 4]
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
  lea rsi, [request_buffer + 7]
  call parse_request_path
  mov rdi, parsed_filename
  call is_filename_safe
  cmp rax, 1
  jne send_400_response
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

string_to_uint:
  xor rax, rax
  xor rcx, rcx
.loop:
  mov cl, byte [rsi]
  cmp cl, 0
  je .done
  cmp cl, '0'
  jl .error
  cmp cl, '9'
  jg .error
  sub cl, '0'
  imul rax, rax, 10
  add rax, rcx
  inc rsi
  jmp .loop
.done:
  ret
.error:
  xor rax, rax
  ret

parse_request_path:
  mov rdi, parsed_filename
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
  sub rdx, 3
.search_loop:
  cmp rcx, rdx
  jge .not_found
  mov eax, dword [request_buffer + rcx]
  cmp eax, 0x0A0D0A0D
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
  mov rsi, O_WRONLY | O_CREAT | O_TRUNC
  mov rdx, 0o644
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
  mov rax, SYS_LSEEK
  mov rdi, [file_descriptor]
  xor rsi, rsi
  mov rdx, 2
  syscall
  mov [file_size], rax
  mov rax, SYS_LSEEK
  mov rdi, [file_descriptor]
  xor rsi, rsi
  xor rdx, rdx
  syscall
  mov rax, SYS_READ
  mov rdi, [file_descriptor]
  mov rsi, file_buffer
  mov rdx, 16384
  syscall
  mov [bytes_read], rax
  mov rax, SYS_CLOSE
  mov rdi, [file_descriptor]
  syscall
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
  mov rax, SYS_LSEEK
  mov rdi, [file_descriptor]
  xor rsi, rsi
  mov rdx, 2
  syscall
  mov [file_size], rax
  mov rax, SYS_CLOSE
  mov rdi, [file_descriptor]
  syscall
  call send_200_headers
  ret

send_file_with_headers:
  call send_200_headers
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, file_buffer
  mov rdx, [bytes_read]
  syscall
  ret

is_filename_safe:
  cmp byte [rdi], 0
  je .unsafe
.check_loop:
  mov al, byte [rdi]
  cmp al, 0
  je .safe
  cmp al, '/'
  je .unsafe
  cmp al, '\'
  je .unsafe
  cmp al, '.'
  jne .continue_loop
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
  mov rdi, rax
  mov rax, SYS_CLOSE
  syscall
  mov rax, 1
  ret
.not_exists:
  xor rax, rax
  ret

copy_string:
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

send_401_response:
  mov rax, SYS_WRITE
  mov rdi, [client_socket]
  mov rsi, http_401_response
  mov rdx, len_401_response
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

print_startup_banner:
  mov rax, SYS_WRITE
  mov rdi, 1
  mov rsi, startup_banner
  mov rdx, len_startup_banner
  syscall
  ret

print_server_ready:
  mov rax, SYS_WRITE
  mov rdi, 1
  mov rsi, server_ready_msg
  mov rdx, len_server_ready_msg
  syscall
  ret

log_request:
  mov rax, SYS_WRITE
  mov rdi, 1
  mov rsi, log_prefix
  mov rdx, len_log_prefix
  syscall
  mov rsi, request_buffer
  mov rcx, 0
.find_newline:
  cmp byte [rsi + rcx], 0x0D
  je .found_newline
  cmp byte [rsi + rcx], 0x0A
  je .found_newline
  inc rcx
  cmp rcx, 100
  jl .find_newline
.found_newline:
  mov rax, SYS_WRITE
  mov rdi, 1
  mov rsi, request_buffer
  mov rdx, rcx
  syscall
  mov rax, SYS_WRITE
  mov rdi, 1
  mov rsi, newline
  mov rdx, 1
  syscall
  ret

socket_error:
  mov rax, SYS_WRITE
  mov rdi, 2
  mov rsi, error_socket
  mov rdx, len_error_socket
  syscall
  jmp exit_error

bind_error:
  mov rax, SYS_WRITE
  mov rdi, 2
  mov rsi, error_bind
  mov rdx, len_error_bind
  syscall
  jmp exit_error

listen_error:
  mov rax, SYS_WRITE
  mov rdi, 2
  mov rsi, error_listen
  mov rdx, len_error_listen
  syscall
  jmp exit_error

exit_error:
  mov rax, SYS_EXIT
  mov rdi, 1
  syscall