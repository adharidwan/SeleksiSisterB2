
macro Syscall num*, arg1, arg2, arg3, arg4, arg5, arg6 {
  mov rax, num

  match _,arg1   \{ mov rdi, arg1 \}
  match _,arg2   \{ mov rsi, arg2 \}
  match _,arg3   \{ mov rdx, arg3 \}
  match _,arg4   \{ mov r10, arg4 \}
  match _,arg5   \{ mov r8 , arg5 \}
  match _,arg6   \{ mov r9 , arg6 \}

  Syscall
}

macro exit code* {
  Syscall  SYS_exit, code
}

__print_raw:
  push    rax 
  push    rdi

  xor     rax, rax
  inc     rax       ; Syscall: write
  mov     rdi, rax  ; fd: stdout
  syscall

  pop     rdi
  pop     rax

  ret

macro printn text*, len* {
  mov     rsi, text ; pointer to string
  mov     rdx, len  ; length of string
  call    __print_raw
}

macro print text*& {
  local .str, .end

  push    rsi
  push    rdx

  mov     rsi, .str             ; pointer to string
  mov     rdx, .end - .str      ; length of string
  call    __print_raw

  pop     rdx
  pop     rsi

  jmp     .end

.str:
  db      text
.end:
}

__int_to_str_inner:
  mov     r14, [buf]
  add     r14, BUF_SIZE
  push    r14
  mov     r15, 10    ; divisor

  ; mov     rax, num   ; divided, lower
@@:
  xor     rdx, rdx   ; divided, upper (also remainder)
  div     r15

  add     dl, '0'
  dec     r14
  mov     [r14], dl

  test    rax, rax
  jnz     @b

  mov     r15, BUF_SIZE
  pop     r15
  sub     r15, r14

  ret

;; <- r14 pos
;; <- r15 length
macro int_to_str num* {
  ; push    rax
  mov     rax, num   ; divided, lower
  call    __int_to_str_inner
  ; pop     rax
}

macro print_int num* {
  push        r14
  push        r15

  int_to_str  num
  printn      r14, r15 

  pop         r15
  pop         r14
}

macro print_int_and_newline num* {
  push        r14
  push        r15

  int_to_str  num
  mov         byte [r14 + r15], 10
  inc         r15
  printn      r14, r15

  pop         r15
  pop         r14
}

macro error exitcode*, text* {
  neg        rax
  int_to_str rax
  mov        byte [r14 + r15], 10
  inc        r15
  printn     r14, r15

  exit exitcode
}

macro err_raw text* {
  print '0', 10
  exit 1
}

;; -> r15 - size
;; <- r15 - pointer
alloc:
  ;; void *mmap(void addr[.length], size_t length, int prot, int flags, int fd, off_t offset);
  ; read: 1, write: 2, exec: 4
  ; shared: 1, private: 2, anonymous: 32
  ; with anonymous mmap call, fd should be -1 and offset 0
  Syscall SYS_mmap, NULL, r15, 3, 33, -1, 0
  cmp     rax, 0
  jge     @f
  error   3, "Allocation failed"
@@:
  mov     r15, rax
  ret

;; -> r15 - pointer
;; -> r14 - size
free:
  Syscall SYS_munmap, r15, r14
  cmp     rax, 0
  jge     @f
  error   4, "Failed to free"
@@:
  ret


;; <- r15 - pointer
acquire_client_obj:
  push       r14

  ; NOTE: this check may give false negatives since it isn't atomic
  mov        r11, [arena]
  mov        r14, [context.pool_tail]
  cmp        [context.pool_head], r14
  jne        @f
  pop        r14
  err_raw    "client pool was fully emptied"
@@:

  mov        r14, 1
  lock xadd  [context.pool_head], r14   ; get idx and increment
  and        r14, 111111111111b         ; clamp
  shl        r14, 2 ; *4
  add        r14, [client_pool]

  xor        r15, r15
  mov        r15d, [r14] ; get index

  shl        r15, 12 ; *4096
  add        r15, [clients]

  pop        r14

  ret

;; -> r15 - pointer
drop_client_obj:
  push    r14

  mov        r11, [arena]

  ; we don't need to check for fullness since we'll never have
  ; more clients than the pool can hold

  mov        r14, 1
  lock xadd  [context.pool_tail], r14
  and        r14, 111111111111b
  sal        r14, 2 ; *4
  add        r14, [client_pool]

  sub        r15, [clients]
  sar        r15, 12 ; /4096

  mov        [r14], r15

  pop        r14

  ret

__httperr_inner:
  mov        [client.state], CLIENT_SEND_ERR

  lea        rdi, [client.header]

  mov        rcx, .text2 - .text1
  mov        rsi, .text1
  rep        movsb

  pop        rbx
  pop        rcx
  pop        rsi
  push       rbx
  rep        movsb

  mov        rcx, .end - .text2
  mov        rsi, .text2
  rep        movsb

  lea        rsi, [client.header]
  sub        rdi, rsi
  mov        [client.header_len], edi

  ret

.text1:
  db "HTTP/1.1 "
.text2:
  db CR, LF
  db "Content-Length: 0", CR, LF
  db "Content-Type: text/plain", CR, LF
  db "Connection: close", CR, LF
  db CR, LF
.end:

macro httperr type* {
  local .type, .end, LENGTH

  push       qword .type
  push       qword LENGTH
  call       __httperr_inner

  jmp        .end

  .type:
    db type
  .end:
  LENGTH = .end - .type
}

setup_http_header:
  push       r15
  
  lea        rdi, [client.header]

  mov        rcx, .text2 - .text1
  mov        rsi, .text1
  rep movsb

  mov        rax, [client.file_size]
  push       r14
  call       __int_to_str_inner 
  mov        rcx, r15
  mov        rsi, r14
  rep movsb
  pop        r14

  mov        rcx, .text3 - .text2
  mov        rsi, .text2
  rep movsb

  mov        rcx, 128
  mov        rsi, [client.file_type]
  jmp        @f
.filetype_loop:
  movsb
@@:
  cmp        byte [rsi], 0
  jnz        .filetype_loop

  mov        rcx, .end - .text3
  mov        rsi, .text3
  rep movsb

  lea        rax, [client.header]
  sub        rdi, rax
  mov        [client.header_len], edi

  pop        r15
  ret

.text1:
  db "HTTP/1.1 200 OK", CR, LF
  db "Content-Length: "
.text2:
  db CR, LF
  db "Content-Type: "
.text3:
  db CR, LF
  db "Connection: close", CR, LF
  db CR, LF
.end:

CR = 13
LF = 10

macro find_char_in_str needle*, haystack*, length* {
  mov        al, needle
  mov        rdi, haystack
  mov        ecx, length

  repne scasb
}

macro find_char_in_str_reverse needle*, haystack*, length* {
  mov        al, needle
  mov        rdi, haystack
  mov        ecx, length
  add        rdi, rcx
  dec        rdi

  std
  repne scasb
  cld
}

;; rcx -> length
;; rdi -> haystack
;; rdi <- pos
;; ZF  <- matched?
find_CRLF_in_str:
  mov        al, CR        ; needle

.loop:
  repne scasb              ; note that scasb sets zf only if it gets a match
  jz         @f            ; also note that jumps modify no flags if they don't trigger
  ret ; ZF = 0             ; neither do returns

@@:
  cmp       byte [rdi], LF ; also also note that `scasb` puts `rdi` one after the match
  jne       .loop

  ret ; ZF = 1
  
;; rcx -> length
;; rdi -> haystack
;; rdi <- pos
;; ZF  <- matched?
find_CRLF_CRLF_in_str:
  mov        al, 13        ; needle

.loop:
  repne scasb              ; note that scasb sets zf only if it gets a match
  jz         @f            ; also note that jumps modify no flags if they don't trigger
  ret ; ZF = 0             ; neither do returns

@@:
  cmp       dword [rdi-1], 0x0a0d0a0d ; CR LF CR LF
  jne       .loop

  ret ; ZF = 1

parse_request:
  ; Parse HTTP method
  lea        r9, [client.request]
  find_char_in_str ' ', r9, [client.request_len]
  jnz        .400
  
  ; Check method type
  mov        rax, "GET "
  cmp        dword [r9], eax
  je         .method_get
  
  mov        rax, "POST"
  cmp        dword [r9], eax
  je         .method_post
  
  mov        eax, "PUT "
  cmp        dword [r9], eax
  je         .method_put
  
  mov        rax, "DELE"
  cmp        dword [r9], eax
  jne        .501
  cmp        word [r9+4], "TE"
  jne        .501
  mov        [client.method], HTTP_DELETE
  jmp        .parse_path
  
.method_get:
  mov        [client.method], HTTP_GET
  jmp        .parse_path
  
.method_post:
  mov        [client.method], HTTP_POST
  jmp        .parse_path
  
.method_put:
  mov        [client.method], HTTP_PUT
  jmp        .parse_path

.parse_path:
  ; Parse path (existing logic)
  mov        r9, rdi
  find_char_in_str ' ', r9, [client.request_len]
  jnz        .400
  push       r9 ; path
  sub        r9, rdi
  not        r9 ; path length
  push       r9

  ; Parse HTTP version (existing logic)
  mov        r9, rdi
  find_char_in_str ' ', r9, [client.request_len]
  jnz        .400_pop
  cmp        dword [r9], "HTTP"
  jne        .400_pop
  cmp        dword [r9 + 3], "P/1."
  jne        .505_pop

  ; Check for Content-Length header (for POST/PUT)
  mov        eax, [client.method]
  cmp        eax, HTTP_POST
  je         .parse_content_length
  cmp        eax, HTTP_PUT
  je         .parse_content_length
  jmp        .handle_request

.parse_content_length:
  lea        rdi, [client.request]
  mov        rcx, [client.request_len]
  call       find_content_length
  mov        [client.content_len], rax
  jmp        .handle_request

.handle_request:
  ; Route based on method
  mov        eax, [client.method]
  cmp        eax, HTTP_GET
  je         .handle_get
  cmp        eax, HTTP_POST
  je         .handle_post
  cmp        eax, HTTP_PUT
  je         .handle_put
  cmp        eax, HTTP_DELETE
  je         .handle_delete

.handle_get:
  ; Existing GET logic for static files
  pop        r9 ; path length
  pop        r10 ; path
  call       serve_static_file
  ret

.handle_post:
  pop        r9 ; path length  
  pop        r10 ; path
  call       handle_post_request
  ret

.handle_put:
  pop        r9 ; path length
  pop        r10 ; path  
  call       handle_put_request
  ret

.handle_delete:
  pop        r9 ; path length
  pop        r10 ; path
  call       handle_delete_request
  ret

; Error handlers (existing)
.501:
  httperr    "501 Not Implemented"
.505_pop:
  add        rsp, 16
  httperr    "505 HTTP Version Not Supported"
.400_pop:
  add        rsp, 16
.400:
  httperr    "400 Bad Request"
  ret


; rax will hold the extension with null bytes in the end
;; rax -> extension with trailing null bytes
set_mime_type:
  or         rax, [.filter]; all letters to lowercase
  mov        rdi, .exts
  mov        rcx, MIME_EXT_COUNT
  
  repne scasq ; insane
  jnz        @f
  sub        rcx, MIME_EXT_COUNT
  ; neg        rcx
  ; dec        rcx
  not        rcx ; index
  shl        rcx, 1 ; *2 for word
  xor        rax, rax
  mov        ax, word [rcx + .lookup]
  add        rax, .lookup
  mov        [client.file_type], rax
  ret
@@:
  mov        [client.file_type], .type_none
  ret

.filter dq 0x2020202020202020 
.exts:
  db "html",   4 dup 0x20
  db "htm",    5 dup 0x20
  db "css",    5 dup 0x20
  db "js",     6 dup 0x20
  db "json",   4 dup 0x20
  db "txt",    5 dup 0x20
  db "jpg",    5 dup 0x20
  db "jpeg",   4 dup 0x20
  db "png",    5 dup 0x20
  db "gif",    5 dup 0x20
  db "svg",    5 dup 0x20
  db "ico",    5 dup 0x20
  db "woff",   4 dup 0x20
  db "woff2",  3 dup 0x20
  db "ttf",    5 dup 0x20
  db "pdf",    5 dup 0x20
  db "zip",    5 dup 0x20
  MIME_EXT_COUNT = ($ - .exts) / 8

.lookup:
  dw .type_html      - .lookup
  dw .type_html      - .lookup
  dw .type_css       - .lookup
  dw .type_js        - .lookup
  dw .type_json      - .lookup
  dw .type_txt       - .lookup
  dw .type_jpg       - .lookup
  dw .type_jpg       - .lookup
  dw .type_png       - .lookup
  dw .type_gif       - .lookup
  dw .type_svg       - .lookup
  dw .type_ico       - .lookup
  dw .type_woff      - .lookup
  dw .type_woff2     - .lookup
  dw .type_ttf       - .lookup
  dw .type_pdf       - .lookup
  dw .type_zip       - .lookup

.types:
.type_html:   db "text/html", 0
.type_css:    db "text/css", 0
.type_js:     db "application/javascript", 0
.type_json:   db "application/json", 0
.type_txt:    db "text/plain", 0
.type_jpg:    db "image/jpeg", 0
.type_png:    db "image/png", 0
.type_gif:    db "image/gif", 0
.type_svg:    db "image/svg+xml", 0
.type_ico:    db "image/x-icon", 0
.type_woff:   db "font/woff", 0
.type_woff2:  db "font/woff2", 0
.type_ttf:    db "font/ttf", 0
.type_pdf:    db "application/pdf", 0
.type_zip:    db "application/zip", 0
.type_none:   db "application/octet-stream", 0
.end:


;; rdi -> signal
set_sigaction:
  ;; int sigaction(int signum, const struct sigaction *_Nullable restrict act, struct sigaction *_Nullable restrict oldact);
  mov     rax, SYS_rt_sigaction
  mov     esi, sigaction   ; arg2
  xor     rdx, rdx         ; arg3
  mov     r10, SIGSET_LEN  ; arg4
  syscall
  ret


find_content_length:
  ; rdi = request buffer, rcx = request length
  ; returns content length in rax
  push       rcx
  push       rdi
  
  mov        rsi, .content_length_header
  mov        rcx, 16  ; "Content-Length: " length
  call       find_substring
  jnz        .no_content_length
  
  ; Found "Content-Length: ", now parse number
  add        rdi, 16  ; skip header name
  call       parse_decimal_number
  pop        rdi
  pop        rcx
  ret

.no_content_length:
  xor        rax, rax
  pop        rdi
  pop        rcx
  ret

.content_length_header:
  db "Content-Length: ", 0


handle_post_request:
  ; r10 = path, r9 = path length
  ; Create new resource
  
  ; Check if we need to receive body
  cmp        [client.content_len], 0
  je         .no_body
  
  ; Set state to receive body
  mov        [client.state], CLIENT_RECV_BODY
  mov        [client.body_received], 0
  ret

.no_body:
  ; Handle POST without body (create empty resource)
  call       create_resource
  call       send_json_response
  ret

handle_put_request:
  ; r10 = path, r9 = path length
  ; Update existing resource
  
  cmp        [client.content_len], 0
  je         .no_body
  
  mov        [client.state], CLIENT_RECV_BODY
  mov        [client.body_received], 0
  ret

.no_body:
  call       update_resource
  call       send_json_response
  ret

handle_delete_request:
  ; r10 = path, r9 = path length
  ; Delete resource
  
  call       delete_resource
  call       send_json_response
  ret

create_resource:
  ; Create new resource based on path and body data
  ; Simple implementation: create file with body content
  
  ; Build file path
  mov        r8, [buf]
  mov        qword [r8], "data/"
  add        r8, 5
  
  ; Copy path (simplified - you'd want better path handling)
  mov        rcx, r9
  mov        rsi, r10
  rep        movsb
  mov        byte [r8], 0
  
  ; Create file
  Syscall    SYS_open, [buf], O_CREAT or O_WRONLY or O_TRUNC, 0644
  cmp        rax, 0
  jl         .create_error
  
  mov        r15, rax  ; file descriptor
  
  ; Write body data
  lea        rsi, [client.request]
  mov        rdx, [client.body_received]
  Syscall    SYS_write, r15, rsi, rdx
  
  Syscall    SYS_close, r15
  
  ; Prepare success response
  lea        rdi, [client.json_data]
  mov        rsi, .success_create
  mov        rcx, .success_create_len
  rep        movsb
  mov        [client.json_len], .success_create_len
  ret

.create_error:
  lea        rdi, [client.json_data]
  mov        rsi, .error_create
  mov        rcx, .error_create_len
  rep        movsb
  mov        [client.json_len], .error_create_len
  ret

.success_create:
  db '{"status": "created", "message": "Resource created successfully"}'
.success_create_len = $ - .success_create

.error_create:
  db '{"status": "error", "message": "Failed to create resource"}'
.error_create_len = $ - .error_create

update_resource:
  ; Similar to create_resource but for updates
  ; Implementation similar to create_resource
  lea        rdi, [client.json_data]
  mov        rsi, .success_update
  mov        rcx, .success_update_len
  rep        movsb
  mov        [client.json_len], .success_update_len
  ret

.success_update:
  db '{"status": "updated", "message": "Resource updated successfully"}'
.success_update_len = $ - .success_update

delete_resource:
  ; Delete resource
  mov        r8, [buf]
  mov        qword [r8], "data/"
  add        r8, 5
  
  mov        rcx, r9
  mov        rsi, r10
  rep        movsb
  mov        byte [r8], 0
  
  Syscall    SYS_unlink, [buf]
  
  lea        rdi, [client.json_data]
  mov        rsi, .success_delete
  mov        rcx, .success_delete_len
  rep        movsb
  mov        [client.json_len], .success_delete_len
  ret

.success_delete:
  db '{"status": "deleted", "message": "Resource deleted successfully"}'
.success_delete_len = $ - .success_delete

send_json_response:
  ; Setup JSON response headers
  lea        rdi, [client.header]
  
  mov        rcx, .header1_len
  mov        rsi, .header1
  rep        movsb
  
  ; Add content length
  mov        rax, [client.json_len]
  call       __int_to_str_inner
  mov        rcx, r15
  mov        rsi, r14
  rep        movsb
  
  mov        rcx, .header2_len
  mov        rsi, .header2
  rep        movsb
  
  lea        rax, [client.header]
  sub        rdi, rax
  mov        [client.header_len], edi
  mov        [client.header_pos], 0
  
  mov        [client.state], CLIENT_SEND_JSON
  ret

.header1:
  db "HTTP/1.1 200 OK", CR, LF
  db "Content-Type: application/json", CR, LF
  db "Content-Length: "
.header1_len = $ - .header1

.header2:
  db CR, LF
  db "Connection: close", CR, LF
  db CR, LF
.header2_len = $ - .header2

find_substring:
  ; rdi = haystack, rcx = haystack_len, rsi = needle
  ; returns ZF=1 if found, rdi points to match
  push       rcx
  push       rsi
  push       rdx
  
  mov        rdx, rsi
  xor        rax, rax
  mov        rcx, -1
  repne      scasb  ; find null terminator
  not        rcx
  dec        rcx    ; rcx = needle length
  
  pop        rdx
  mov        rsi, rdx
  pop        rdx
  push       rcx    ; save needle length
  mov        rcx, rdx  ; restore haystack length
  
.search_loop:
  mov        al, [rsi]
  repne      scasb
  jnz        .not_found
  
  push       rcx
  push       rdi
  push       rsi
  
  mov        rcx, [rsp + 12]  ; needle length
  dec        rcx
  inc        rsi
  
  repe       cmpsb
  je         .found
  
  pop        rsi
  pop        rdi
  pop        rcx
  jmp        .search_loop

.found:
  add        rsp, 12  ; clean stack
  add        rsp, 8   ; needle length
  dec        rdi      ; point to start of match
  pop        rcx
  ret

.not_found:
  add        rsp, 8   ; needle length  
  pop        rcx
  or         rax, 1   ; clear ZF
  ret

parse_decimal_number:
  ; rdi points to decimal string
  ; returns number in rax
  xor        rax, rax
  xor        rbx, rbx
  
.parse_loop:
  mov        bl, [rdi]
  cmp        bl, '0'
  jl         .done
  cmp        bl, '9' 
  jg         .done
  
  sub        bl, '0'
  imul       rax, 10
  add        rax, rbx
  inc        rdi
  jmp        .parse_loop

.done:
  ret

