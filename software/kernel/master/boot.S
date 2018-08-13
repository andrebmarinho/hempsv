        .section .text.init   
        .globl  entry
entry:	
        li sp, sp_addr
        la t0, L2
        csrw 0x305, t0

        jal     main
        nop
L1:
        j L1
        nop

L2:
        j L2
        nop
      