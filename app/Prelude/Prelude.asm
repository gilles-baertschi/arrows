alloc:
push rbp
mov rbp, rsp
mov rax, 12
xor rdi, rdi
syscall
push rax
mov rdi, [rbp+16]
add rdi, rax
mov rax, 12
syscall
pop rax
mov rsp, rbp
pop rbp
ret

id:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rsp, rbp
pop rbp
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

swap:
push rbp
mov rbp, rsp
push 16
call alloc
mov rdi, [rbp+16]
mov rdx, [rdi+8]
mov [rax], rdx
mov rdx, [rdi]
mov [rax+8], rdx
mov rsp, rbp
pop rbp
ret

swap_choice:
push rbp
mov rbp, rsp
push 16
call alloc
mov rdi, [rbp+16]
mov rdx, [rdi]
xor rdx, 1
mov [rax], rdx
mov rdx, [rdi+8]
mov [rax+8], rdx
mov rsp, rbp
pop rbp
ret

reorder_to_front:
; (a, (b, c)) -> ((a, b), c)
push rbp
mov rbp, rsp
push 16
call alloc
mov rdx, rax
push 16
call alloc
mov rdi, [rbp+16]
mov rsi, [rdi+8]
; rdx -> (a, b)
; rax -> ((a, b), c)
; rdi -> (a, (b, c))
; rsi -> (b, c)
mov rcx, [rdi]
mov [rdx], rcx
mov rcx, [rsi]
mov [rdx+8], rcx
mov [rax], rdx
mov rcx, [rsi+8]
mov [rax+8], rcx
mov rsp, rbp
pop rbp
ret

reorder_to_back:
; ((a, b), c) -> (a, (b, c))
push rbp
mov rbp, rsp
push 16
call alloc
mov rdx, rax
push 16
call alloc
mov rdi, [rbp+16]
mov rsi, [rdi]
; rdx -> (b, c)
; rax -> (a, (b, c))
; rdi -> ((a, b), c)
; rsi -> (a, b)
mov rcx, [rsi+8]
mov [rdx], rcx
mov rcx, [rdi+8]
mov [rdx+8], rcx
mov [rax+8], rdx
mov rcx, [rsi]
mov [rax], rcx
mov rsp, rbp
pop rbp
ret

; (c, (a | b)) -> ((c, a) | (c, b)) 
include_left:
push rbp
mov rbp, rsp
push 16
call alloc
mov rdx, rax
push 16
call alloc
mov rdi, [rbp+16]
mov rsi, [rdi+8]
mov rcx, [rsi]
mov [rax], rcx
mov [rax+8], rdx
mov rcx, [rdi]
mov [rdx], rcx
mov rcx, [rsi+8]
mov [rdx+8], rcx
mov rsp, rbp
pop rbp
ret

; ((a | b), c) -> ((a, c) | (b, c)) 
include_right:
push rbp
mov rbp, rsp
push 16
call alloc
mov rdx, rax
push 16
call alloc
mov rdi, [rbp+16]
mov rsi, [rdi]
mov rcx, [rsi]
mov [rax], rcx
mov [rax+8], rdx
mov rcx, [rdi+8]
mov [rdx+8], rcx
mov rcx, [rsi+8]
mov [rdx], rcx
mov rsp, rbp
pop rbp
ret

add_int:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
add rax, rcx
mov rsp, rbp
pop rbp
ret

sub_int:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
sub rax, rcx
mov rsp, rbp
pop rbp
ret

mul_int:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
imul rcx
mov rsp, rbp
pop rbp
ret

div_int:
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

neg_int:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
neg rax
mov rsp, rbp
pop rbp
ret

abs_int:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, rax
neg rcx
cmovge rax, rcx
mov rsp, rbp
pop rbp
ret

add_float:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rdx, [rax]
movsd xmm0, [rdx]
mov rdx, [rax+8]
movsd xmm1, [rdx]
addsd xmm0, xmm1
sub rsp, 8
push 8
call alloc
add rsp, 8
movq rdx, xmm0
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

sub_float:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rdx, [rax]
movsd xmm0, [rdx]
mov rdx, [rax+8]
movsd xmm1, [rdx]
subsd xmm0, xmm1
sub rsp, 8
push 8
call alloc
add rsp, 8
movq rdx, xmm0
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

mul_float:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rdx, [rax]
movsd xmm0, [rdx]
mov rdx, [rax+8]
movsd xmm1, [rdx]
mulsd xmm0, xmm1
sub rsp, 8
push 8
call alloc
add rsp, 8
movq rdx, xmm0
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

div_float:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rdx, [rax]
movsd xmm0, [rdx]
mov rdx, [rax+8]
movsd xmm1, [rdx]
divsd xmm0, xmm1
sub rsp, 8
push 8
call alloc
add rsp, 8
movq rdx, xmm0
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

float_to_int:
push rbp
mov rbp, rsp
mov rdx, [rbp+16]
cvttsd2si rax, [rdx]
mov rsp, rbp
pop rbp
ret

abs_float:
push rbp
mov rbp, rsp
finit
mov rdx, [rbp+16]
fld qword [rdx]
fabs
sub rsp, 8
fistp qword [rsp]
push 8
call alloc
add rsp, 8
pop rdx
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

neg_float:
push rbp
mov rbp, rsp
finit
mov rdx, [rbp+16]
fld qword [rdx]
fchs
sub rsp, 8
fistp qword [rsp]
push 8
call alloc
add rsp, 8
pop rdx
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

cos:
push rbp
mov rbp, rsp
finit
mov rdx, [rbp+16]
fld qword [rdx]
fcos
sub rsp, 8
fistp qword [rsp]
push 8
call alloc
add rsp, 8
pop rdx
mov [rax], rdx
mov rsp, rbp
pop rbp
ret

tan:
push rbp
mov rbp, rsp
finit
mov rdx, [rbp+16]
fld qword [rdx]
fptan
sub rsp, 8
fistp qword [rsp]
push 8
call alloc
add rsp, 8
pop rdx
mov [rax], rdx
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

xor:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rcx, [rax+8]
mov rax, [rax]
xor rax, rcx
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
pop rbp
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

less_eq_int:
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

greater_eq_int:
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

eq_float:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rdx, [rax]
movsd xmm0, [rdx]
mov rdx, [rax+8]
movsd xmm1, [rdx]
comisd xmm0, xmm1
sete al
movzx rax, al
mov rsp, rbp
pop rbp
ret

less_float:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rdx, [rax]
movsd xmm0, [rdx]
mov rdx, [rax+8]
movsd xmm1, [rdx]
comisd xmm0, xmm1
setl al
movzx rax, al
mov rsp, rbp
pop rbp
ret

greater_float:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rdx, [rax]
movsd xmm0, [rdx]
mov rdx, [rax+8]
movsd xmm1, [rdx]
comisd xmm0, xmm1
setg al
movzx rax, al
mov rsp, rbp
pop rbp
ret

less_eq_float:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rdx, [rax]
movsd xmm0, [rdx]
mov rdx, [rax+8]
movsd xmm1, [rdx]
comisd xmm0, xmm1
setle al
movzx rax, al
mov rsp, rbp
pop rbp
ret

greater_eq_float:
push rbp
mov rbp, rsp
mov rax, [rbp+16]
mov rdx, [rax]
movsd xmm0, [rdx]
mov rdx, [rax+8]
movsd xmm1, [rdx]
comisd xmm0, xmm1
setge al
movzx rax, al
mov rsp, rbp
pop rbp
ret

; is_left:
; push rbp
; mov rbp, rsp
; mov rax, [rbp+16]
; mov rax, [rax]
; test rax, rax
; setz al
; mov rax, al
; mop rsp, rbp
; pop rbp
; ret
;
; is_right:
; push rbp
; mov rbp, rsp
; mov rax, [rbp+16]
; mov rax, [rax]
; test rax, rax
; setnz al
; mov rax, al
; mop rsp, rbp
; pop rbp
; ret

composition:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 0
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
push qword [rbp+16]
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
mov qword [rax], 0
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
mov qword [rax], 0
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
mov rdi, [rdi]
mov [rax], rdi
mov rsp, rbp
pop rbp
ret

triple_asterisk:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 0
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
add rsp, 16
push rax
mov rdi, [rbp+24]
push qword [rdi+8]
mov rdi, [rbp+16]
push qword [rdi+8]
call call
add rsp, 16
mov rdx, rax
push 16
call alloc
add rsp, 8
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
mov qword [rax], 0
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
add rsp, 16
push rax
mov rdi, [rbp+24]
push qword [rdi+8]
push qword [rbp+16]
call call
add rsp, 16
mov rdx, rax
push 16
add rsp, 8
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
mov qword [rax], 0
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
mov [rax+8], rdx
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
mov qword [rax], 0
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
jz .case_right
push qword [rbp+24]
push qword [rdi+8]
call call
mov rdx, rax
push 16
call alloc
mov qword [rax], 1
mov [rax+8], rdx
jmp .done_right
.case_right:
mov rax, [rbp+16]
.done_right:
mov rsp, rbp
pop rbp
ret

triple_plus:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 0
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
jnz .case_right
mov rdx, [rbp+24]
push qword [rdx]
push qword [rdi+8]
call call
mov rdx, rax
push 16
call alloc
mov qword [rax], 0
mov [rax+8], rdx
jmp .done_left
.case_right:
mov rdx, [rbp+24]
push qword [rdx+8]
push qword [rdi+8]
call call
mov rdx, rax
push 16
call alloc
mov qword [rax], 1
mov [rax+8], rdx
.done_left:
mov rsp, rbp
pop rbp
ret

triple_bar:
push rbp
mov rbp, rsp
push 24
call alloc
mov qword [rax], 0
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
je .extend
mov rax, rdi
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
