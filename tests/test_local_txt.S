.global _start
_start:

    mov x0, #1
    adr x1, msg
    mov x2, len
    mov x8, #64 // write()
    svc #0
l:
    mov x0, #0
    mov x8, #93 // exit()
    b l
    svc #0
msg:
    .ascii "Hello World!\n"
len = . - msg
