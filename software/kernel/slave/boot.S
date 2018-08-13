.section .text.init
.globl entry
entry:
	li sp, sp_addr
	# ecall
	call main

end:
	j end
	nop

.org 0x3c
#address 0x3c
interrupt_service_routine_entry:  
	j       interrupt_service_routine
	nop

#address 0x44
system_service_routine_entry:  
        j       system_service_routine 
        nop

#address 0x4c
interrupt_service_routine:
	csrrw a7, mscratch, a7 	#save a7 in mscratch, then ignore old mscratch...
	la a7, current 		#get current value (will be in a7)
	lw a7, 0(a7)

	#SAVE ra AND REGISTERS                                   
	sw x1, 	  0(a7) 	# reg[0]  = ra 
	sw x2, 	  4(a7) 	# reg[1]  = sp
	sw x4, 	  8(a7) 	# reg[2]  = tp
	sw x5, 	 12(a7) 	# reg[3]  = t0
	sw x6, 	 16(a7) 	# reg[4]  = t1
	sw x7, 	 20(a7) 	# reg[5]  = t2
	sw x8, 	 24(a7) 	# reg[6]  = s0/fp
	sw x9, 	 28(a7) 	# reg[7]  = s1
	sw x10,  32(a7) 	# reg[8]  = a0 
	sw x11,  36(a7) 	# reg[9]  = a1
	sw x12,  40(a7) 	# reg[10] = a2
	sw x13,  44(a7) 	# reg[11] = a3
	sw x14,  48(a7) 	# reg[12] = a4
	sw x15,  52(a7) 	# reg[13] = a5
	sw x16,  56(a7) 	# reg[14] = a6
	# sw x17,  60(a7) 	# reg[15] = a7 #will be saved later!
	sw x18,  64(a7) 	# reg[16] = s2
	sw x19,  68(a7) 	# reg[17] = s3
	sw x20,  72(a7) 	# reg[18] = s4
	sw x21,  76(a7) 	# reg[19] = s5
	sw x22,  80(a7) 	# reg[20] = s6
	sw x23,  84(a7) 	# reg[21] = s7
	sw x24,  88(a7) 	# reg[22] = s8
	sw x25,  92(a7) 	# reg[23] = s9
	sw x26,  96(a7) 	# reg[24] = s10
	sw x27, 100(a7) 	# reg[25] = s11
	sw x28, 104(a7) 	# reg[26] = t3
	sw x29, 108(a7) 	# reg[27] = t4
	sw x30, 112(a7) 	# reg[28] = t5
	sw x31, 116(a7) 	# reg[29] = t6
	
	#ignore x3 (gp)	

	# Now that everything is saved, PC will be:
	csrr t0, mepc
	sw t0, 120(a7)
	mv t0, zero

	# Finally, we can save a7 on memory!
	csrrw a7, mscratch, a7
	# using another register which value was already saved
	la t0, current 		#get current value (will be in t0)
	lw t0, 0(t0)
	sw a7, 60(t0) #mscratch[15] = a7
	
	# TODO: sp 
	li sp, sp_addr 
    
	#JUMP TO INTERRUPT SERVICE ROUTINE
	li     a1, 0x20000020
	lw      a0, 0(a1)     #IRQ_STATUS
	li	   a3, 0x20000010    
	lw      a2, 0(a3)     #IRQ_MASK
	and     a0,a0,a2

	jal     OS_InterruptServiceRoutine
####################################################

system_service_routine:
	#mscratch must have (old) current value
	csrrw t6, mscratch, t6 	#save t6 in mscratch, then ignore old mscratch...
	la t6, current 		#get current value (will be in t6)
	lw t6, 0(t6)
	
	# saves minimum context
	sw x10,  32(t6) 	# reg[8]  = a0 
	sw x11,  36(t6) 	# reg[9]  = a1
	sw x12,  40(t6) 	# reg[10] = a2
	sw x13,  44(t6) 	# reg[11] = a3
	sw x14,  48(t6) 	# reg[12] = a4
	sw x15,  52(t6) 	# reg[13] = a5
	sw x16,  56(t6) 	# reg[14] = a6
	sw x17,  60(t6) 	# reg[15] = a7

	sw x1, 	  0(t6) 	# reg[0]  = ra 
	sw x2, 	  4(t6) 	# reg[1]  = sp

	# Save PC:
	csrr t0, mepc
	sw t0, 120(t6)

	csrrw t6, mscratch, t6 # restore t6 <= t6_old and mscratch <= current

	# TODO: sp for each page...
	li sp, sp_addr
	# lw sp, 128(a7) 

	# jumps to system calls handler
	jal     Syscall
	nop
		
	# verifies if scheduling is needed
	la      t1, schedule_after_syscall
	lw      t1, 0(t1)
	beqz    t1, system_service_restore 
	nop
	
	# saves the context of the running task
	# la a7, current
	# lw a7, 0(a7)
	jal     ASM_SaveRemainingContext
	nop
	
	# schedules the next ready task
	jal     Scheduler
	nop
	
	# restores the context and runs the scheduled task

	la a0, current
	lw a0, 0(a0)

	jal     ASM_RunScheduledTask        
	nop
       
system_service_restore:
	# "current" address is in mscratch 
	csrrw t5, mscratch, t5

	# restores the context previously saved
	lw      x1,    0(t5)    # ra 
	lw      x2,    4(t5)    # sp

	# lw      x10,  32(t5)    # a0  # dont restore a0, it contains Syscall return...
	lw      x11,  36(t5)    # a1
	lw      x12,  40(t5)    # a2
	lw      x13,  44(t5)    # a3
	lw      x14,  48(t5)    # a4
	lw      x15,  52(t5)    # a5
	lw      x16,  56(t5)    # a6  
	lw      x17,  60(t5)    # a7

	lw 		t6,	 120(t5)	# first, set desired pc to mepc
	csrw  mepc,      t6 
							# then, recover page offset.    
	lw      t1,  124(t5)   
	li		t2, 0x20000190
	sw		t1, 0(t2) #NEXT_PAGE

	mret        
###################################################
        
.globl  ASM_SaveRemainingContext             
ASM_SaveRemainingContext:
	# "current" address is in mscratch 
	# csrrw a7, mscratch, a7
	la a7, current
	lw a7, 0(a7)

	#SAVE ra AND REGISTERS             
	# Its necessary to save Syscall return. 
	sw x10,  32(a7) 	# reg[8]  = a0 # saves SystemCall return 

	sw x8, 	 24(a7) 	# reg[6]  = s0/fp
	sw x9, 	 28(a7) 	# reg[7]  = s1
	sw x18,  64(a7) 	# reg[16] = s2
	sw x19,  68(a7) 	# reg[17] = s3
	sw x20,  72(a7) 	# reg[18] = s4
	sw x21,  76(a7) 	# reg[19] = s5
	sw x22,  80(a7) 	# reg[20] = s6
	sw x23,  84(a7) 	# reg[21] = s7
	sw x24,  88(a7) 	# reg[22] = s8
	sw x25,  92(a7) 	# reg[23] = s9
	sw x26,  96(a7) 	# reg[24] = s10
	sw x27, 100(a7) 	# reg[25] = s11
		
	jr ra
	nop
                
###################################################

# .globl ASM_SetCurrent
# ASM_SetCurrent:
# 	csrw mscratch, a0 # 0x340
# 	jr ra
# ###################################################

.globl  ASM_SaveStackPointer        
ASM_SaveStackPointer:
	sw sp, 4(a0)
	jr ra
###################################################

.globl  ASM_RunScheduledTask        
ASM_RunScheduledTask:     
	lw a7, 60(a0)
	csrrw a7, mscratch, a7 # stores a7 in mscratch
						   # then we can use a7 to recover context
	mv a7, a0

	lw 		a0,	 120(a7)	# first, set desired pc to mepc
	csrw  mepc,      a0 
							# then, recover page offset.    
	lw      a1,  124(a7)    
	li		a2, 0x20000190
	sw		a1, 0(a2) #NEXT_PAGE

	# Finally: recover scheduled task context
 
	lw      x1,    0(a7)    # ra 
	lw      x2,    4(a7)    # sp

	bnez x2, dont_set_initial_sp # if sp is not zero continue, else, loads from initial sp
	# lw      x2,  128(a7) 
	li sp, sp_addr

dont_set_initial_sp:
	lw      x4,    8(a7)    # tp
	lw      x5,   12(a7)    # t0
	lw      x6,   16(a7)    # t1
	lw      x7,   20(a7)    # t2
	lw      x8,   24(a7)    # s0/fp
	lw      x9,   28(a7)    # s1
	lw      x10,  32(a7)    # a0  
	lw      x11,  36(a7)    # a1
	lw      x12,  40(a7)    # a2
	lw      x13,  44(a7)    # a3
	lw      x14,  48(a7)    # a4
	lw      x15,  52(a7)    # a5
	lw      x16,  56(a7)    # a6  
	# lw      x17,  60(a7)    # a7 #recover from csr
	lw      x18,  64(a7)    # s2
	lw      x19,  68(a7)    # s3
	lw      x20,  72(a7)    # s4
	lw      x21,  76(a7)    # s5
	lw      x22,  80(a7)    # s6
	lw      x23,  84(a7)    # s7
	lw      x24,  88(a7)    # s8
	lw      x25,  92(a7)    # s9
	lw      x26,  96(a7)    # s10  
	lw      x27, 100(a7)    # s11
	lw      x28, 104(a7)    # t3  
	lw      x29, 108(a7)    # t4
	lw      x30, 112(a7)    # t5
	lw      x31, 116(a7)    # t6

	#lw 		x3, 120(a7)

	csrrw a7, mscratch, a7

	# as MRET is executed, mpie wil be loaded to mie
	
	mret # jumps to mepc which has target task pc now
###################################################

	.globl  ASM_SetInterruptEnable
ASM_SetInterruptEnable:
	# only used in kernel initialization.
	# mstatus.mpie bit is set, so, ASM_RunScheduledTask
	# will load mstatus.mie <= mstatus.mpie in mret.

	li t0, 0x80 #set mpie
	csrw mstatus, t0 #0x300

	li t0, 0x800 #set meie
	csrw mie, t0 #0x304

	li t0, 0x3c
	csrw mtvec, t0 #0x305
        
	jr ra	

################## READ_INST ################## 

# 	.globl  read_inst
# read_inst:        
# 	li      a7, global_inst
                                   
# 	mfc0    $26,$16                 
# 	sw      $26,0(a7)    #  save global_inst

# 	jr      ra             # return to the caller 
# 	nop 
        
        