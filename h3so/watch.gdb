set pagination off
watch *0x5501691
set $n = 1
while $n > 0
  continue
  info reg
  x/20xb $eip-10 
  bt
  set $n = $n - 1
end
exit