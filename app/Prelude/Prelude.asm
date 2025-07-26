alloc:
push rbp
mov rbp, rsp
mov rax, 12
xor rdi, rdi
syscall
mov rdi, [rbp+16]
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
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rax, [rax]
mov rsp, rbp
pop rbp
ret

snd:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rax, [rax+8]
mov rsp, rbp
pop rbp
ret

double:
push rbp
mov rbp, rsp
push 16
call alloc
mov rdi, [rbp+16]
mov [rax], rdi
mov [rax+8], rdi
mov rsp, rbp
pop rbp
ret

put_char:
push rbp
mov rbp, rsp
mov rax, 1
mov rdi, 1
mov rsi, rbp
add rsi, 16
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
push rbp
mov rbp, rsp
mov rdx, [rbp+16]
push qword [rdx] 
push qword [rdx+8]
call call
mov rsp, rbp
pop rbp
ret

add:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
add rax, rcx
mov rsp, rbp
pop rbp
ret

sub:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
sub rax, rcx
mov rsp, rbp
pop rbp
ret

mul:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
imul rcx
mov rsp, rbp
pop rbp
ret

div:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
xor edx, edx
idiv rcx
mov rsp, rbp
pop rbp
ret

mod:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
xor edx, edx
idiv rcx
mov rax, rdx
mov rsp, rbp
pop rbp
ret

not:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
xor rax, 1
mov rsp, rbp
pop rbp
ret

and:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
and rax, rcx
mov rsp, rbp
pop rbp
ret

or:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
or rax, rcx
mov rsp, rbp
pop rbp
ret

bool_to_choice:
push rbp
mov rbp, rsp
push 16
call alloc
mov rdx, [rbp+16]
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

l:
push rbp
mov rbp, rsp
push 16
call alloc
mov qword [rax], 0
mov rdx, [rbp+16]
mov [rax+8], rdx
mov rsp, rbp
pop rbp
ret

r:
push rbp
mov rbp, rsp
push 16
call alloc
mov qword [rax], 1
mov rdx, [rbp+16]
mov [rax+8], rdx
mov rsp, rbp
pop rbp
ret

eq:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
cmp rax, rcx
sete al
movzx rax, al
mov rsp, rbp
ret

less_int:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
cmp rax, rcx
setl al
movzx rax, al
mov rsp, rbp
pop rbp
ret

greater_int:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
cmp rax, rcx
setg al
movzx rax, al
mov rsp, rbp
pop rbp
ret

less_equ_int:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
cmp rax, rcx
setle al
movzx rax, al
mov rsp, rbp
pop rbp
ret

greater_equ_int:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
cmp rax, rcx
setge al
movzx rax, al
mov rsp, rbp
pop rbp
ret

composition:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 1
mov rdi, [rbp+16]
mov [rax+8], rdi
lea rdi, composition_inner
mov [rax+16], rdi
mov rsp, rbp
pop rbp
ret

composition_inner:
push rbp
mov rbp, rsp
mov rdi, [rbp+24]
push qword [rdi]
push qword [rdi+16]
call call
mov rdi, [rbp+24]
push qword [rdi+8]
push qword rax
call call
mov rsp, rbp
pop rbp
ret

first:
; ...|ret|ext fp|
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 1
mov rdi, [rbp+16]
mov [rax+8], rdi
lea rdi, first_inner
mov [rax+16], rdi
mov rsp, rbp
pop rbp
ret

first_inner:
; ...|ret|arg|ext fp|
push rbp
mov rbp, rsp
push qword [rbp+24]
mov rdi, [rbp+16]
push qword [rdi]
call call
mov rdx, rax
push 16
call alloc
mov [rax], rdx
mov rdi, [rbp+16]
mov rdi, [rdi+8]
mov [rax+8], rdi
mov rsp, rbp
pop rbp
ret

second:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 1
mov rdi, [rbp+16]
mov [rax+8], rdi
lea rdi, second_inner
mov [rax+16], rdi
mov rsp, rbp
pop rbp
ret

second_inner:
push rbp
mov rbp, rsp
push qword [rbp+24]
mov rdi, [rbp+16]
push qword [rdi+8]
call call
mov rdx, rax
push 16
call alloc
mov [rax+8], rdx
mov rdi, [rbp+16]
mov rdi, [rdi+8]
mov [rax], rdi
mov rsp, rbp
pop rbp
ret

triple_asterisk:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 1
mov rdi, [rbp+16]
mov [rax+8], rdi
lea rdi, triple_asterisk_inner
mov [rax+16], rdi
mov rsp, rbp
pop rbp
ret

triple_asterisk_inner:
push rbp
mov rbp, rsp
mov rdi, [rbp+24]
push qword [rdi]
mov rdi, [rbp+16]
push qword [rdi]
call call
push rax
mov rdi, [rbp+24]
push qword [rdi+8]
mov rdi, [rbp+16]
push qword [rdi+8]
call call
mov rdx, rax
push 16
call alloc
mov [rax+8], rdx
pop rdx
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

triple_and:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 1
mov rdi, [rbp+16]
mov [rax+8], rdi
lea rdi, triple_and_inner
mov [rax+16], rdi
mov rsp, rbp
pop rbp
ret

triple_and_inner:
push rbp
mov rbp, rsp
mov rdi, [rbp+24]
push qword [rdi]
push qword [rbp+16]
call call
push rax
mov rdi, [rbp+24]
push qword [rdi+8]
push qword [rbp+16]
call call
mov rdx, rax
push 16
call alloc
mov [rax+8], rdx
pop rdx
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

left:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 1
mov rdi, [rbp+16]
mov [rax+8], rdi
lea rdi, left_inner
mov [rax+16], rdi
mov rsp, rbp
pop rbp
ret

left_inner:
push rbp
mov rbp, rsp
mov rdi, [rbp+16]
mov rcx, [rdi]
test rcx, rcx
jnz .case_left
push qword [rbp+24]
push qword [rdi+8]
call call
mov rdx, rax
push 16
call alloc
mov qword [rax], 0
mov [rax], rdx
jmp .done_left
.case_left:
mov rax, [rbp+16]
.done_left:
mov rsp, rbp
pop rbp
ret

right:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 1
mov rdi, [rbp+16]
mov [rax+8], rdi
lea rdi, right_inner
mov [rax+16], rdi
mov rsp, rbp
pop rbp
ret

right_inner:
push rbp
mov rbp, rsp
mov rdi, [rbp+16]
mov rcx, [rdi]
test rcx, rcx
jz .case_left
push qword [rbp+24]
push qword [rdi+8]
call call
mov rdx, rax
push 16
call alloc
mov qword [rax], 1
mov [rax], rdx
jmp .done_left
.case_left:
mov rax, [rbp+16]
.done_left:
mov rsp, rbp
pop rbp
ret

triple_plus:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 1
mov rdi, [rbp+16]
mov [rax+8], rdi
lea rdi, triple_plus_inner
mov [rax+16], rdi
mov rsp, rbp
pop rbp
ret

triple_plus_inner:
push rbp
mov rbp, rsp
mov rdi, [rbp+16]
mov rcx, [rdi]
test rcx, rcx
jnz .case_left
mov rdx, [rbp+24]
push qword [rdx]
push qword [rdi+8]
call call
mov rdx, rax
push 16
call alloc
mov qword [rax], 0
mov [rax], rdx
jmp .done_left
.case_left:
mov rdx, [rbp+24]
push qword [rdx+8]
push qword [rdi+8]
call call
mov rdx, rax
push 16
call alloc
mov qword [rax], 1
mov [rax], rdx
.done_left:
mov rsp, rbp
pop rbp
ret

triple_bar:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 1
mov rdi, [rbp+16]
mov [rax+8], rdi
lea rdi, triple_bar_inner
mov [rax+16], rdi
mov rsp, rbp
pop rbp
ret

triple_bar_inner:
push rbp
mov rbp, rsp
mov rdi, [rbp+16]
mov rcx, [rdi]
test rcx, rcx
jnz .case_left
mov rdx, [rbp+24]
push qword [rdx]
push qword [rdi+8]
call call
jmp .done_left
.case_left:
mov rdx, [rbp+24]
push qword [rdx+8]
push qword [rdi+8]
call call
.done_left:
mov rsp, rbp
pop rbp
ret

call:
; ...|ret|arg|ext fp|
push rbp
mov rbp, rsp
mov rdi, [rbp+24]
mov rax, [rdi]
cmp rax, 0
jne .extend
mov rax, [rdi+8]
jmp .done
.extend:
push qword [rdi+8]
mov rax, [rdi+16]
.done:
push qword [rbp+16]
call rax
mov rsp, rbp
pop rbp
ret
