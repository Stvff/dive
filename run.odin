package dive

import "core:intrinsics"

/* TODO: make all instructions that rely on registers implicitly do that explicitly.
         examples are: add, sub, mul, div, if, ifn */
run :: proc(program: []Tac) #no_bounds_check {
	NIL :: Word{}
	stack_allocated: int
	ip, sp: int
	regs: [REGISTER_AMOUNT]Word
	stack: [STACK_SIZE]Word
	defer for i in 0..<stack_allocated{
		word := stack[i]
		printf("0x%x: 0x%x, 0d%v, f%v, f%v\n", i*8, word.i, word.i, word.float_4B, word.float_8B)
	}

	main_loop: for {
		defer ip += 1
		op_code := program[ip].op_code
		a0 := program[ip].args[0]
		a1 := program[ip].args[1]
		a2 := program[ip].args[2]
//		printf("ip: 0x%x, sp: 0x%x, stack_allocated: 0x%x\n", ip, 8*sp, 8*stack_allocated)
//		print_tac(program[ip])
	switch op_code {
	case .ERR:
		panic("error instruction")
/* move register to register */
	case .MOV_1B_R2R:
		regs[a0.i] = NIL
		regs[a0.i].one_byte = regs[a1.i].one_byte
	case .MOV_2B_R2R:
		regs[a0.i] = NIL
		regs[a0.i].two_byte = regs[a1.i].two_byte
	case .MOV_4B_R2R:
		regs[a0.i] = NIL
		regs[a0.i].four_byte = regs[a1.i].four_byte
	case .MOV_8B_R2R:
		regs[a0.i] = NIL
		regs[a0.i].eight_byte = regs[a1.i].eight_byte
/* move register to memory */
	case .MOV_1B_R2M:
		stack[sp + a0.i/8].eight_byte[a0.i%8] = regs[a1.i].one_byte
	case .MOV_2B_R2M:
		stack[sp + a0.i/8].four_two_byte[a0.i%4] = regs[a1.i].two_byte
	case .MOV_4B_R2M:
		stack[sp + a0.i/8].two_four_byte[a0.i%2] = regs[a1.i].four_byte
	case .MOV_8B_R2M:
		stack[sp + a0.i/8].eight_byte = regs[a1.i].eight_byte
/* move memory to register */
	case .MOV_1B_M2R:
		regs[a0.i] = NIL
		regs[a0.i].one_byte = stack[sp + a1.i/8].eight_byte[a1.i%8]
	case .MOV_2B_M2R:
		regs[a0.i] = NIL
		regs[a0.i].two_byte = stack[sp + a1.i/8].four_two_byte[a1.i%4]
	case .MOV_4B_M2R:
		regs[a0.i] = NIL
		regs[a0.i].four_byte = stack[sp + a1.i/8].two_four_byte[a1.i%2]
	case .MOV_8B_M2R:
		regs[a0.i] = NIL
		regs[a0.i].eight_byte = stack[sp + a1.i/8].eight_byte
/* move memory to memory */
	case .MOV_1B_M2M:
		stack[sp + a0.i/8].eight_byte[a0.i%8] = stack[sp + a1.i/8].eight_byte[a1.i%8]
	case .MOV_2B_M2M:
		stack[sp + a0.i/8].four_two_byte[a0.i%4] = stack[sp + a1.i/8].four_two_byte[a1.i%4]
	case .MOV_4B_M2M:
		stack[sp + a0.i/8].two_four_byte[a0.i%2] = stack[sp + a1.i/8].two_four_byte[a1.i%2]
	case .MOV_8B_M2M:
		stack[sp + a0.i/8].eight_byte = stack[sp + a1.i/8].eight_byte
/* move immediate value to register */
	case .MOV_I2R:
		regs[a0.i] = a1
	case .MOV_1B_I2M:
		stack[sp + a0.i/8].eight_byte[a0.i%8] = a1.one_byte
	case .MOV_2B_I2M:
		stack[sp + a0.i/8].four_two_byte[a0.i%4] = a1.two_byte
	case .MOV_4B_I2M:
		stack[sp + a0.i/8].two_four_byte[a0.i%2] = a1.four_byte
	case .MOV_8B_I2M:
		stack[sp + a0.i/8].eight_byte = a1.eight_byte

/* casts */
	case .CAST_F4_TO_S8: regs[a0.i].sint_8B = cast(i64) regs[a1.i].float_4B
	case .CAST_S8_TO_F4: regs[a0.i].float_4B = cast(f32) regs[a1.i].sint_8B
	case .CAST_F8_TO_S8: regs[a0.i].sint_8B = cast(i64) regs[a1.i].float_8B
	case .CAST_S8_TO_F8: regs[a0.i].float_8B = cast(f64) regs[a1.i].sint_8B
	case .CAST_F4_TO_F8: regs[a0.i].float_8B = cast(f64) regs[a1.i].float_4B
	case .CAST_F8_TO_F4: regs[a0.i].float_4B = cast(f32) regs[a1.i].float_8B

/* heap operations */
	case .READ_1B:
		stack[sp + a0.i/8].eight_byte[a0.i%8] = (cast(^byte) stack[sp + a1.i/8].ptr_8B)^
	case .READ_2B:
		stack[sp + a0.i/8].four_two_byte[a0.i%4] = (cast(^[2]byte) stack[sp + a1.i/8].ptr_8B)^
	case .READ_4B:
		stack[sp + a0.i/8].two_four_byte[a0.i%2] = (cast(^[4]byte) stack[sp + a1.i/8].ptr_8B)^
	case .READ_8B:
		stack[sp + a0.i/8].eight_byte = (cast(^[8]byte) stack[sp + a1.i/8].ptr_8B)^
	case .WRITE_1B:
		p := cast(^byte) stack[sp + a0.i/8].ptr_8B
		p^ = stack[sp + a1.i/8].eight_byte[a1.i%8]
	case .WRITE_2B:
		p := cast(^[2]byte) stack[sp + a0.i/8].ptr_8B
		p^ = stack[sp + a1.i/8].four_two_byte[a1.i%4]
	case .WRITE_4B:
		p := cast(^[4]byte) stack[sp + a0.i/8].ptr_8B
		p^ = stack[sp + a1.i/8].two_four_byte[a1.i%2]
	case .WRITE_8B:
		p := cast(^[8]byte) stack[sp + a0.i/8].ptr_8B
		p^ = stack[sp + a1.i/8].eight_byte
	case .ADDR:
		stack[sp + a0.i/8].ptr_8B = &stack[sp + a1.i/8].eight_byte[a1.i%8]

/* procedure calling and stack handling */
	case .WINDUP:
		sp += a0.i
		stack[sp].i = sp - a0.i
		stack[sp + 1].i = ip + a1.i + 1
  	/* FIXME: has to raise an error when out of mem */
	case .PUSH:
		ZERO_WORD :: Word{}
		stack_slice := stack[stack_allocated:stack_allocated + a0.i]
		stack_slice[0] = ZERO_WORD
		for i := 1; i < len(stack_slice); i *= 2 {
			copy(stack_slice[i:], stack_slice[:i])
		}
		stack_allocated += a0.i
	case .POP:
		stack_allocated -= a0.i
	case .REWIND:
		sp = stack[sp].i

/* arithmetic */
	/* add */
	case .ADD_UINT:
		regs[0].uint_8B = regs[1].uint_8B + regs[2].uint_8B
	case .ADD_SINT:
		regs[0].sint_8B = regs[1].sint_8B + regs[2].sint_8B
	case .ADD_F32:
		regs[0].float_4B = regs[1].float_4B + regs[2].float_4B
	case .ADD_F64:
		regs[0].float_8B = regs[1].float_8B + regs[2].float_8B
	/* sub */
	case .SUB_UINT:
		regs[0].uint_8B = regs[1].uint_8B - regs[2].uint_8B
	case .SUB_SINT:
		regs[0].sint_8B = regs[1].sint_8B - regs[2].sint_8B
	case .SUB_F32:
		regs[0].float_4B = regs[1].float_4B - regs[2].float_4B
	case .SUB_F64:
		regs[0].float_8B = regs[1].float_8B - regs[2].float_8B
	/* mul */
	case .MUL_UINT:
		regs[0].uint_8B = regs[1].uint_8B * regs[2].uint_8B
	case .MUL_SINT:
		regs[0].sint_8B = regs[1].sint_8B * regs[2].sint_8B
	case .MUL_F32:
		regs[0].float_4B = regs[1].float_4B * regs[2].float_4B
	case .MUL_F64:
		regs[0].float_8B = regs[1].float_8B * regs[2].float_8B
	/* div */
	case .DIV_UINT:
		regs[0].uint_8B = regs[1].uint_8B / regs[2].uint_8B
	case .DIV_SINT:
		regs[0].sint_8B = regs[1].sint_8B / regs[2].sint_8B
	case .DIV_F32:
		regs[0].float_4B = regs[1].float_4B / regs[2].float_4B
	case .DIV_F64:
		regs[0].float_8B = regs[1].float_8B / regs[2].float_8B

/* comparisons */
	case .GROWS_UINT:
		if regs[1].uint_8B < regs[2].uint_8B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .GROWS_SINT:
		if regs[1].sint_8B < regs[2].sint_8B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .GROWS_F32:
		if regs[1].float_4B < regs[2].float_4B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .GROWS_F64:
		if regs[1].float_8B < regs[2].float_8B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .SHNKS_UINT:
		if regs[1].uint_8B > regs[2].uint_8B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .SHNKS_SINT:
		if regs[1].sint_8B > regs[2].sint_8B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .SHNKS_F32:
		if regs[1].float_4B > regs[2].float_4B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .SHNKS_F64:
		if regs[1].float_8B > regs[2].float_8B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .EQU_UINT:
		if regs[1].uint_8B == regs[2].uint_8B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .EQU_SINT:
		if regs[1].sint_8B == regs[2].sint_8B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .EQU_F32:
		if regs[1].float_4B == regs[2].float_4B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
	case .EQU_F64:
		if regs[1].float_8B == regs[2].float_8B do regs[0].sint_8B = 1
		else do regs[0].sint_8B = 0
/* conditional jumps */
	case .JUMP_IF:
		if regs[0].sint_8B != 0 do ip = a0.i - 1
	case .JUMP_IFN:
		if regs[0].sint_8B == 0 do ip = a0.i - 1

	case .JUMP:
		ip = a0.i - 1
	case .DYN_JUMP:
		ip = stack[sp + a0.i/8].i - 1

	case .SYSCALL_0:
		regs[a0.i].uptr_8B = intrinsics.syscall(regs[a1.i].uptr_8B)
	case .SYSCALL_1:
		base := sp + regs[a2.i].i/8
		sys_arg1 := stack[base].uptr_8B
		regs[a0.i].uptr_8B = intrinsics.syscall(regs[a1.i].uptr_8B, sys_arg1)
	case .SYSCALL_2:
		base := sp + regs[a2.i].i/8
		sys_arg1 := stack[base].uptr_8B
		sys_arg2 := stack[base + 1].uptr_8B
		regs[a0.i].uptr_8B = intrinsics.syscall(regs[a1.i].uptr_8B, sys_arg1, sys_arg2)
	case .SYSCALL_3:
		base := sp + regs[a2.i].i/8
		sys_arg1 := stack[base].uptr_8B
		sys_arg2 := stack[base + 1].uptr_8B
		sys_arg3 := stack[base + 2].uptr_8B
		regs[a0.i].uptr_8B = intrinsics.syscall(regs[a1.i].uptr_8B, sys_arg1, sys_arg2, sys_arg3)
	case .SYSCALL_4:
		base := sp + regs[a2.i].i/8
		sys_arg1 := stack[base].uptr_8B
		sys_arg2 := stack[base + 1].uptr_8B
		sys_arg3 := stack[base + 2].uptr_8B
		sys_arg4 := stack[base + 3].uptr_8B
		regs[a0.i].uptr_8B = intrinsics.syscall(regs[a1.i].uptr_8B, sys_arg1, sys_arg2, sys_arg3, sys_arg4)
	case .SYSCALL_5:
		base := sp + regs[a2.i].i/8
		sys_arg1 := stack[base].uptr_8B
		sys_arg2 := stack[base + 1].uptr_8B
		sys_arg3 := stack[base + 2].uptr_8B
		sys_arg4 := stack[base + 3].uptr_8B
		sys_arg5 := stack[base + 4].uptr_8B
		regs[a0.i].uptr_8B = intrinsics.syscall(regs[a1.i].uptr_8B, sys_arg1, sys_arg2, sys_arg3, sys_arg4, sys_arg5)
	case .SYSCALL_6:
		base := sp + regs[a2.i].i/8
		sys_arg1 := stack[base].uptr_8B
		sys_arg2 := stack[base + 1].uptr_8B
		sys_arg3 := stack[base + 2].uptr_8B
		sys_arg4 := stack[base + 3].uptr_8B
		sys_arg5 := stack[base + 4].uptr_8B
		sys_arg6 := stack[base + 5].uptr_8B
		regs[a0.i].uptr_8B = intrinsics.syscall(regs[a1.i].uptr_8B, sys_arg1, sys_arg2, sys_arg3, sys_arg4, sys_arg5, sys_arg6)

	case .DONE:
		break main_loop
	}}
}
