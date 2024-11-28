extern printf
section .note.GNU-stack
section .data
section .bss
section .text
  global fib
fib:
  push rbp
  mov rbp, rsp
  push rbx
  add rsp, 0x18
  xor rax, rax
  mov [rbp - 8], rax
  mov [rbp - 16], rax
  inc qword [rbp - 16]
loop:
  mov rax, [rbp - 8]
  mov rbx, [rbp - 16]
  add rax, rbx
  mov [rbp - 16], rax
  mov [rbp - 8], rbx
  dec rdi
  cmp rdi, 0
  jne loop
  mov rax, [rbp - 8]
  sub rsp, 0x18
  pop rbx
  leave
  ret
