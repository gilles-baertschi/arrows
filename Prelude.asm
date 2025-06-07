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
