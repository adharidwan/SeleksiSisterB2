format ELF64 executable 3

entry start

; ===================================================================
;                            CODE SEGMENT
; ===================================================================
segment readable executable

  ; --- Constants & Syscalls ---
  AF_INET     = 2
  SOCK_STREAM = 1
  O_RDONLY    = 0
  O_WRONLY    = 1
  O_CREAT     = 64
  O_TRUNC     = 512

  SYS_READ    = 0
  SYS_WRITE   = 1
  SYS_OPEN    = 2
  SYS_CLOSE   = 3
  SYS_SOCKET  = 41
  SYS_ACCEPT  = 43
  SYS_BIND    = 49
  SYS_LISTEN  = 50
  SYS_FORK    = 57
  SYS_EXIT    = 60
  SYS_UNLINK  = 87

start:
  ; Create socket
  mov rax, SYS_SOCKET
  mov rdi, AF_INET
  mov rsi, SOCK_STREAM
  xor rdx, rdx
  syscall
  mov r12, rax ; Save listener socket fd

  ; Prepare sockaddr_in structure for binding
  mov word [sockaddr_in], AF_INET
  mov word [sockaddr_in+2], 0x901F ; Port 8080 (big-endian)
  mov dword [sockaddr_in+4], 0      ; Listen on all interfaces

  ; Bind socket
  mov rax, SYS_BIND
  mov rdi, r12
  mov rsi, sockaddr_in
  mov rdx, 16
  syscall

  ; Listen for connections
  mov rax, SYS_LISTEN
  mov rdi, r12
  mov rsi, 5 ; Backlog 5
  syscall

accept_loop:
  ; Accept a connection
  mov rax, SYS_ACCEPT
  mov rdi, r12
  xor rsi, rsi
  xor rdx, rdx
  syscall
  mov r13, rax ; Save client socket fd

  ; Fork to handle the new client
  mov rax, SYS_FORK
  syscall

  cmp rax, 0
  je  child_process ; Child process handles the request

parent_process:
  ; Parent closes client socket and loops back to accept
  mov rax, SYS_CLOSE
  mov rdi, r13
  syscall
  jmp accept_loop

child_process:
  ; Child closes listening socket
  mov rax, SYS_CLOSE
  mov rdi, r12
  syscall

  ; Read request from client
  mov rax, SYS_READ
  mov rdi, r13
  mov rsi, request_buffer
  mov rdx, 2048
  syscall
  mov r15, rax ; Save request length

  ; --- HTTP Method Routing ---
  mov eax, dword [request_buffer]
  cmp eax, 'POST'
  je handle_post
  cmp eax, 'PUT '
  je handle_put
  cmp eax, 'DELE'
  je handle_del
  cmp eax, 'GET '
  je handle_get

  jmp handle_405 ; Unsupported method

; --- Filename Parser ---
parse_filename:
  cmp byte [rsi], '/'
  jne .parse_loop ; Doesn't start with '/', parse immediately
  inc rsi ; Skip leading '/'

.parse_loop:
  mov al, byte [rsi]
  cmp al, ' '
  je  .found_eof
  mov byte [rdi], al
  inc rsi
  inc rdi
  jmp .parse_loop

.found_eof:
  mov byte [rdi], 0
  ret

; --- Method Handlers ---
handle_post:
  xor rcx, rcx
.find_body_loop:
  mov eax, dword [request_buffer + rcx]
  cmp eax, 0x0A0D0A0D ; CRLFCRLF
  je .found_body
  inc rcx
  cmp rcx, r15
  jl .find_body_loop
  jmp handle_400

.found_body:
  lea rbx, [request_buffer + rcx + 4] ; rbx = pointer to body
  mov rdx, r15
  sub rdx, rcx
  sub rdx, 4 ; rdx = length of body
  mov r15, rdx

.write_post_to_file:
  mov rax, SYS_OPEN
  mov rdi, post_filename
  mov rsi, O_WRONLY or O_CREAT or O_TRUNC
  mov rdx, 420 ; File permissions (644 octal = 420 decimal)
  syscall
  mov r14, rax

  mov rax, SYS_WRITE
  mov rdi, r14 ; file descriptor
  mov rsi, rbx ; body pointer
  mov rdx, r15 ; body length
  syscall

  mov rax, SYS_CLOSE
  mov rdi, r14
  syscall

  mov rsi, http_201
  mov rdx, len_201
  jmp send_response

handle_del:
  lea rsi, [request_buffer + 7] ; "DELETE "
  lea rdi, [parsed_filename]
  call parse_filename

  lea rdi, [parsed_filename]
  call is_filename_safe
  cmp rax, 1
  jne handle_400

  mov rax, SYS_UNLINK
  lea rdi, [parsed_filename]
  syscall

  cmp rax, 0
  jne handle_404 ; If unlink failed, file not found

  mov rsi, http_200
  mov rdx, len_200
  jmp send_response

handle_put:
  lea rsi, [request_buffer + 4] ; "PUT "
  lea rdi, [parsed_filename]
  call parse_filename

  lea rdi, [parsed_filename]
  call is_filename_safe
  cmp rax, 1
  jne handle_400

  xor rcx, rcx
.find_body_loop_put:
  mov eax, dword [request_buffer + rcx]
  cmp eax, 0x0A0D0A0D
  je .found_body_put
  inc rcx
  cmp rcx, r15
  jl .find_body_loop_put
  jmp handle_400

.found_body_put:
  lea rbx, [request_buffer + rcx + 4]
  mov rdx, r15
  sub rdx, rcx
  sub rdx, 4
  mov r15, rdx

  mov rax, SYS_OPEN
  lea rdi, [parsed_filename]
  mov rsi, O_WRONLY or O_CREAT or O_TRUNC
  mov rdx, 420 ; File permissions (644 octal = 420 decimal)
  syscall
  mov r14, rax

  mov rax, SYS_WRITE
  mov rdi, r14
  mov rsi, rbx
  mov rdx, r15
  syscall

  mov rax, SYS_CLOSE
  mov rdi, r14
  syscall

  mov rsi, http_200
  mov rdx, len_200
  jmp send_response

handle_get:
  lea rsi, [request_buffer + 4]
  lea rdi, [parsed_filename]
  call parse_filename

  cmp byte [parsed_filename], 0
  jne .validate_filename

  lea rdi, [file_root] ; Default to index.html
  jmp serve_file

.validate_filename:
  lea rdi, [parsed_filename]
  call is_filename_safe
  cmp rax, 1
  jne handle_400

  lea rdi, [parsed_filename]
  jmp serve_file

; --- File and Response Logic ---
serve_file:
  mov rax, SYS_OPEN
  mov rsi, O_RDONLY
  xor rdx, rdx
  syscall
  cmp rax, 0
  jl handle_404
  mov r14, rax ; file descriptor

  mov rax, SYS_READ
  mov rdi, r14
  mov rsi, file_buffer
  mov rdx, 8192
  syscall
  mov r15, rax ; bytes read

  mov rax, SYS_CLOSE
  mov rdi, r14
  syscall

  ; Send 200 OK header
  mov rax, SYS_WRITE
  mov rdi, r13
  mov rsi, http_200
  mov rdx, len_200
  syscall

  ; Send file content
  mov rax, SYS_WRITE
  mov rdi, r13
  mov rsi, file_buffer
  mov rdx, r15
  syscall
  jmp close_and_exit

handle_400:
  mov rsi, http_400
  mov rdx, len_400
  jmp send_response
handle_404:
  mov rsi, http_404
  mov rdx, len_404
  jmp send_response
handle_405:
  mov rsi, http_405
  mov rdx, len_405
  jmp send_response

send_response:
  mov rax, SYS_WRITE
  mov rdi, r13
  syscall
  jmp close_and_exit

close_and_exit:
  mov rax, SYS_CLOSE
  mov rdi, r13
  syscall
  mov rax, SYS_EXIT
  xor rdi, rdi
  syscall

; --- Security Function ---
is_filename_safe:
  ; Check for an empty string.
  cmp byte [rdi], 0
  je  .unsafe

.check_loop:
  mov al, byte [rdi]
  cmp al, 0
  je  .safe ; End of string, all checks passed

  ; Check 1: Disallow any forward slashes ('/').
  cmp al, '/'
  je  .unsafe

  ; Check 2: Look for a dot ('.').
  cmp al, '.'
  jne .continue_loop

  ; If we found a dot, check for an adjacent dot ("..").
  cmp byte [rdi + 1], '.'
  je  .unsafe

.continue_loop:
  inc rdi
  jmp .check_loop

.safe:
  mov rax, 1
  ret

.unsafe:
  xor rax, rax
  ret

; ===================================================================
;                            DATA SEGMENT
; ===================================================================
segment readable writeable

  file_root db 'index.html', 0
  file_test db 'test.html', 0

  ; HTTP Headers
  http_200    db 'HTTP/1.1 200 OK', 0Dh, 0Ah, 'Content-Type: text/html', 0Dh, 0Ah, 0Dh, 0Ah
  len_200     = $ - http_200

  post_filename db 'post_result.txt', 0
  http_201    db 'HTTP/1.1 201 Created', 0Dh, 0Ah, 0Dh, 0Ah, 'File created successfully.'
  len_201     = $ - http_201

  http_400    db 'HTTP/1.1 400 Bad Request', 0Dh, 0Ah, 0Dh, 0Ah, '<h1>400 Bad Request</h1>'
  len_400     = $ - http_400

  http_404    db 'HTTP/1.1 404 Not Found', 0Dh, 0Ah, 0Dh, 0Ah, '<h1>404 Not Found</h1>'
  len_404     = $ - http_404

  http_405    db 'HTTP/1.1 405 Method Not Allowed', 0Dh, 0Ah, 0Dh, 0Ah, 'Method Not Allowed.'
  len_405     = $ - http_405

  sockaddr_in     rb 16
  request_buffer  rb 2048
  file_buffer     rb 8192
  parsed_filename rb 256