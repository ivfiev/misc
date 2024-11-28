extern printf
extern atoi
extern fib
section .note.GNU-stack
section .data
  fmt db "%d",10,0
section .bss
section .text
  global main
main:
  push rbp
  mov rbp, rsp
  sub rsp, 0x10
  mov rdi, 0
  mov rsi, rsp
  mov rdx, 0xF
  mov rax, 0
  syscall
  mov rdi, rsp
  call atoi
  mov rdi, rax
  call fib
  lea rdi, [fmt]
  mov rsi, rax
  mov rax, 1
  call printf
  leave
  ret

