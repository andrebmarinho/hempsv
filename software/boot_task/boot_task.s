.section .text
.globl entry

entry:
        #li      sp,sp_addr              # set up the stack pointer, using a constant defined in the linker script.
        #using page stack pointer from TCB...

        jal     main
        nop

        move a0, zero
        #Chamada de sistema (syscall)
        ecall
        nop
       
end:
        j end                    # loop when finished if there is no environment to return to.
        nop
################################################

   .globl SystemCall
SystemCall:
   
   ecall
   nop
   jr	ra
   nop
   