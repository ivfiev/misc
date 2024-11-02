set pagination off
watch *0xffdd6b98
set $n = 2
while $n > 0
  continue
  info reg
  bt
  disas
  x/20xb $pc-10
  set $n = $n - 1
end
exit