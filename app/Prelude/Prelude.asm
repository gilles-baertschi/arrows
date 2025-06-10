alloc:
push rbp
mov rbp, rsp
push rax
mov rax, 12
xor rdi, rdi
syscall
mov rdi, [rsp]
add rdi, rax
mov rax, 12
syscall
mov rax, rdi
mov rsp, rbp
pop rbp
ret

id:
ret

fst:
mov rax, [rax]
ret

snd:
mov rax, [rax+8]
ret

double:
push rax
mov rax, 16
call alloc
mov rdi, rax
pop rax
mov [rdi], rax
mov [rdi+8], rax
mov rax, rdi
ret

put_char:
push rbp
mov rbp, rsp
push rax
mov rax, 1
mov rdi, 1
mov rsi, rsp
mov rdx, 1
syscall
mov rsp, rbp
pop rbp
ret

read_char:
push rbp
mov rbp, rsp
mov rax, 0
push rax
mov rdi, 0
mov rsi, rsp
mov rdx, 1
syscall
pop rax
mov rsp, rbp
pop rbp
ret

app:
mov rdi, [rax]
mov rax, [rax+8]
call rdi
ret

add:
mov rcx, [rax+8]
mov rax, [rax]
add rax, rcx
ret

sub:
mov rcx, [rax+8]
mov rax, [rax]
sub rax, rcx
ret

mul:
mov rcx, [rax+8]
mov rax, [rax]
imul rcx
ret

div:
mov rcx, [rax+8]
mov rax, [rax]
idiv rcx
ret

not:
xor rax, 1
ret

and:
mov rcx, [rax+8]
mov rax, [rax]
and rax, rcx
ret

or:
mov rcx, [rax+8]
mov rax, [rax]
or rax, rcx
ret
