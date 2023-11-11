package dive

/* Calling convention:
   CALL should first allocate memory for the return variables, then for
   the proc parameters, and then for the current instruction and stack
   pointers.
   The proc itself then allocates the space for its internal variables.
   On RETURN, the proc cleans up its own memory, and jumps back to the
   calling location using the information provided. The callee then moves
   the values from the return variables into the variables designated by
   I_CALL, and POST_CALL cleans up the last memory.
Stackshape:
    previous_stack_pointer, post_call_location,
    out_parameters_8B_aligned in declaration order,
    in_parameters_8B_aligned in declaration order,
    inbody_vars_1B, alignment padding to 8B,
    inbody_vars_2B, alignment padding to 8B,
    inbody_vars_4B, alignment padding to 8B,
    inbody_vars_8B,
*/

generate_bytecode :: proc(scope: ^Scope, prev_var_places: ^Var_places, stack_offsets: ^Var_ams) -> (block: Block) {
	stack_offsets := stack_offsets
	new_stack_offsets: Var_ams
	if stack_offsets == nil do stack_offsets = &new_stack_offsets
	if scope.kind == .PROC || scope.kind == .GLOB {/* figuring out the stack picture */
		amounts: Var_ams
		collect_variables(scope, &amounts, true)
//		println(amounts)
		stack_offsets.vars_1B = 8*(2 + len(scope.parameters_input) + len(scope.parameters_output))
		stack_offsets.vars_2B = stack_offsets.vars_1B + amounts.vars_1B
		stack_offsets.vars_2B += (8 - stack_offsets.vars_2B%8) % 8
		stack_offsets.vars_4B = stack_offsets.vars_2B + 2*amounts.vars_2B
		stack_offsets.vars_4B += (8 - stack_offsets.vars_4B%8) % 8
		stack_offsets.vars_8B = stack_offsets.vars_4B + 4*amounts.vars_4B
		stack_offsets.vars_8B += (8 - stack_offsets.vars_8B%8) % 8

		stack_offsets.frame_size = stack_offsets.vars_8B + 8*amounts.vars_8B
		local_stack_push := (stack_offsets.frame_size - stack_offsets.vars_1B)/8
		if scope.kind == .PROC do add_tac(&block, .PUSH, Word{i = local_stack_push})
		else do add_tac(&block, .PUSH, Word{i = local_stack_push + 2})
	}
//	println(stack_offsets)

	block_infos: map[Name]Block_info
	blocs: map[Name]Block
	procs: map[Name]Block
	var_places := Var_places {
		scope_above = prev_var_places,
	}
	defer{
		delete(var_places.vars)
		delete(block_infos)
		for key, value in blocs {
			delete(value.program)
			delete(value.labels_still_unresolved)
			delete(value.labels_to_be_offset)
		}
		for key, value in procs {
			delete(value.program)
			delete(value.labels_still_unresolved)
			delete(value.labels_to_be_offset)
		}
		delete(blocs)
		delete(procs)
	}
//	base_stack_offsets := stack_offsets^
	if scope.kind == .PROC {/* define where the output and input parameters live */
		o := 16
		for name, i in scope.parameters_output {
			decf := scope.decfineds[name]
			var_places.vars[name] = {decf.type, o + 8*i}
		}
		o = 16 + 8*len(scope.parameters_output)
		for name, i in scope.parameters_input {
			decf := scope.decfineds[name]
			var_places.vars[name] = {decf.type, o + 8*i}
		}
	}
	for name in scope.names_inbody {/* figure our where the vars are on the stack */
		decf := scope.decfineds[name]
		switch decf.type {
			case .T_UNKNOWN:
			case .T_BYTE, .T_U8, .T_S8:
				var_places.vars[name] = {
					decf.type,
					stack_offsets.vars_1B
				}
				stack_offsets.vars_1B += 1
			case .T_U16, .T_S16:
				var_places.vars[name] = {
					decf.type,
					stack_offsets.vars_2B
				}
				stack_offsets.vars_2B += 2
			case .T_U32, .T_S32, .T_F32:
				var_places.vars[name] = {
					decf.type,
					stack_offsets.vars_4B
				}
				stack_offsets.vars_4B += 4
			case .T_U64, .T_S64, .T_F64, .T_PTR, .T_WORD:
				var_places.vars[name] = {
					decf.type,
					stack_offsets.vars_8B
				}
				stack_offsets.vars_8B += 8
			case .T_TYPE, .T_INSTCN:
			case .T_PROC, .T_BLOC:
			case .T_INT, .T_FLOAT, .T_NAME:
		}
	}
	for name in scope.names_inbody {/* pre-compile inner scopes	*/
		decf := scope.decfineds[name]
		if decf.type == .T_BLOC {
			new_block := generate_bytecode(decf.content.(^Scope), &var_places, stack_offsets)
			blocs[name] = new_block
		} else if decf.type == .T_PROC {
			new_block := generate_bytecode(decf.content.(^Scope), &var_places, nil)
			procs[name] = new_block
		}
	}
//	println(var_places)
//	println(stack_offsets)

	gen_loop: for statement in scope.statements {
		if len(statement.right) == 0 do continue gen_loop
//		println(statement.left, statement.right)
		long_assignment := false

		switch q in statement.right[0] {
		case Value:
			if len(statement.left) == 1 {
				left := retrieve_var_place(statement.left[0], &var_places)
				add_MOV_2M_tac(left.place, left.type, statement.right[0], &block, &var_places, scope)
				continue gen_loop
			} else do long_assignment = true
		case Name:
			if len(statement.left) == 1 {
				left := retrieve_var_place(statement.left[0], &var_places)
				add_MOV_2M_tac(left.place, left.type, statement.right[0], &block, &var_places, scope)
				continue gen_loop
			} else do long_assignment = true
		case Type: panic("No idea how to deal with a type in a codegen statement")
		case Instruction: switch q {
		case .I_UNKNOWN: panic("Unknown instruction in codegen")
		case .I_CAST: /* OPTIM: Turn ints of same size into single movs */
			left := retrieve_var_place(statement.left[0], &var_places)
			right_type := type_of_valthing(statement.right[1], scope)
			add_MOV_2R_tac(1, right_type, statement.right[1], &block, &var_places, scope)
			lt, rt := left.type, right_type
			if lt >= .T_U8 && lt <= .T_S64 && rt == .T_F32 do add_tac(&block, .CAST_F4_TO_S8, Word{i=0}, Word{i=1})
			else if lt == .T_F32 && rt >= .T_U8 && rt <= .T_S64 do add_tac(&block, .CAST_S8_TO_F4, Word{i=0}, Word{i=1})
			else if lt >= .T_U8 && lt <= .T_S64 && (rt == .T_F64 || rt == .T_FLOAT) do add_tac(&block, .CAST_F8_TO_S8, Word{i=0}, Word{i=1})
			else if lt == .T_F64 && rt >= .T_U8 && rt <= .T_S64 do add_tac(&block, .CAST_S8_TO_F8, Word{i=0}, Word{i=1})
			else if lt == .T_F64 && rt == .T_F32 do add_tac(&block, .CAST_F4_TO_F8, Word{i=0}, Word{i=1})
			else if lt == .T_F32 && (rt == .T_F64 || rt == .T_F64) do add_tac(&block, .CAST_F8_TO_F4, Word{i=0}, Word{i=1})
			add_MOV_R2M_tac(left.place, left.type, 0, &block)
		case .I_TRANS: /* OPTIM: Turn vars of same size into single movs */
			left := retrieve_var_place(statement.left[0], &var_places)
			right_type := type_of_valthing(statement.right[1], scope)
			add_MOV_2R_tac(0, right_type, statement.right[1], &block, &var_places, scope)
			add_MOV_R2M_tac(left.place, left.type, 0, &block)
		case .I_READ:
			left := retrieve_var_place(statement.left[0], &var_places)
			right := retrieve_var_place(statement.right[1].(Name), &var_places)
			op_code := Op_code.READ_1B + log2[size_of_type[left.type]]
			add_tac(&block, op_code, Word{i=left.place}, Word{i=right.place})
		case .I_WRITE:
			left := retrieve_var_place(statement.right[1].(Name), &var_places)
			right := retrieve_var_place(statement.right[2].(Name), &var_places)
			op_code := Op_code.WRITE_1B + log2[size_of_type[right.type]]
			add_tac(&block, op_code, Word{i=left.place}, Word{i=right.place})
		case .I_ADDR:
			left := retrieve_var_place(statement.left[0], &var_places)
			right := retrieve_var_place(statement.right[1].(Name), &var_places)
			add_tac(&block, .ADDR, Word{i=left.place}, Word{i=right.place})
		case .I_CALL:
			to_be_called_decf, _ := visible_decfined(scope, statement.right[1].(Name))
			tbc := to_be_called_decf.content.(^Scope)
			smol_push := 2 + len(tbc.parameters_output) + len(tbc.parameters_input)
			add_tac(&block, .PUSH, Word{i = smol_push})
			offset_to_input_params := stack_offsets.frame_size + 8*(2 + len(tbc.parameters_output))
			for it in 2..<len(statement.right) {
				right_type := type_of_valthing(statement.right[it], scope)
				add_MOV_2M_tac(offset_to_input_params + 8*(it - 2), right_type, statement.right[it], &block, &var_places, scope)
			}
			add_tac(&block, .WINDUP, Word{i = stack_offsets.frame_size/8}, Word{i = 1})
			add_tac(&block, .JUMP, Word{/* will be filled out later */})
			unres := Unresolved_name{
				false,
				statement.right[1].(Name),
				{ len(block.program) - 1, 0 }
			}
			append(&block.labels_still_unresolved, unres)
			add_tac(&block, .REWIND)
			offset_to_output_params := stack_offsets.frame_size + 16
			for it in 0..<len(statement.left) {
				left := retrieve_var_place(statement.left[it], &var_places)
				add_MOV_M2M_tac(left.place, left.type, offset_to_output_params + 8*it, &block)
			}
			add_tac(&block, .POP, Word{i = smol_push})
		case .I_RETURN:
			caller_mem := stack_offsets.vars_1B/8
			add_tac(&block, .POP, Word{i = stack_offsets.frame_size/8 - caller_mem})
			add_tac(&block, .DYN_JUMP, Word{i = 8})
		case .I_ADD, .I_SUB, .I_MUL, .I_DIV: /* OPTIM: ADD and SUB etc that operate on stack vars too */
			op_codes := [?]Op_code{.ADD_UINT, .SUB_UINT, .MUL_UINT, .DIV_UINT}
			left := retrieve_var_place(statement.left[0], &var_places)
			add_MOV_2R_tac(1, left.type, statement.right[1], &block, &var_places, scope)
			add_MOV_2R_tac(2, left.type, statement.right[2], &block, &var_places, scope)
			lt := left.type
			op_code := op_codes[statement.right[0].(Instruction) - Instruction.I_ADD]
			if lt == .T_S8 || lt == .T_S16 || lt == .T_S32 || lt == .T_S64 do op_code += Op_code(1)
			else if lt == .T_F32 do op_code += Op_code(2)
			else if lt == .T_F64 do op_code += Op_code(3)
			add_tac(&block, op_code)
			add_MOV_R2M_tac(left.place, left.type, 0, &block)
		case .I_GROWS:
			right_type := type_of_valthing(statement.right[1], scope)
			add_MOV_2R_tac(1, right_type, statement.right[1], &block, &var_places, scope)
			add_MOV_2R_tac(2, right_type, statement.right[2], &block, &var_places, scope)
			op_code, rt := Op_code.GROWS_SINT, right_type
			if rt == .T_U8 || rt == .T_U16 || rt == .T_U32 || rt == .T_U64 do op_code = .GROWS_UINT
			else if rt == .T_F32 do op_code = .GROWS_F32
			else if rt == .T_F64 do op_code = .GROWS_F64
			add_tac(&block, op_code)
			left := retrieve_var_place(statement.left[0], &var_places)
			add_MOV_R2M_tac(left.place, left.type, 0, &block)
		case .I_SHNKS:
			right_type := type_of_valthing(statement.right[1], scope)
			add_MOV_2R_tac(1, right_type, statement.right[1], &block, &var_places, scope)
			add_MOV_2R_tac(2, right_type, statement.right[2], &block, &var_places, scope)
			op_code, rt := Op_code.SHNKS_SINT, right_type
			if rt == .T_U8 || rt == .T_U16 || rt == .T_U32 || rt == .T_U64 do op_code = .SHNKS_UINT
			else if rt == .T_F32 do op_code = .SHNKS_F32
			else if rt == .T_F64 do op_code = .SHNKS_F64
			add_tac(&block, op_code)
			left := retrieve_var_place(statement.left[0], &var_places)
			add_MOV_R2M_tac(left.place, left.type, 0, &block)
		case .I_EQU:
			right_type := type_of_valthing(statement.right[1], scope)
			add_MOV_2R_tac(1, right_type, statement.right[1], &block, &var_places, scope)
			add_MOV_2R_tac(2, right_type, statement.right[2], &block, &var_places, scope)
			op_code, rt := Op_code.EQU_SINT, right_type
			if rt == .T_U8 || rt == .T_U16 || rt == .T_U32 || rt == .T_U64 do op_code = .EQU_UINT
			else if rt == .T_F32 do op_code = .EQU_F32
			else if rt == .T_F64 do op_code = .EQU_F64
			add_tac(&block, op_code)
			left := retrieve_var_place(statement.left[0], &var_places)
			add_MOV_R2M_tac(left.place, left.type, 0, &block)
		case .I_IF:
			right_type := type_of_valthing(statement.right[1], scope)
			add_MOV_2R_tac(0, right_type, statement.right[1], &block, &var_places, scope)
			add_tac(&block, .JUMP_IF, Word{/* will be filled out later */})
			unres := Unresolved_name{
				false,
				statement.right[2].(Name),
				{ len(block.program) - 1, 0 }
			}
			append(&block.labels_still_unresolved, unres)
		case .I_IFN:
			right_type := type_of_valthing(statement.right[1], scope)
			add_MOV_2R_tac(0, right_type, statement.right[1], &block, &var_places, scope)
			add_tac(&block, .JUMP_IFN, Word{/* will be filled out later */})
			unres := Unresolved_name{
				false,
				statement.right[2].(Name),
				{ len(block.program) - 1, 0 }
			}
			append(&block.labels_still_unresolved, unres)
		case .I_SKIP:
			add_tac(&block, .JUMP, Word{/* will be filled out later */})
			unres := Unresolved_name{
				true,
				statement.right[1].(Name),
				{ len(block.program) - 1, 0 }
			}
			append(&block.labels_still_unresolved, unres)
		case .I_LABEL:
			bloc_for_insert := blocs[statement.right[1].(Name)]
			offset := len(block.program)
			block_infos[statement.right[1].(Name)] = {offset, len(bloc_for_insert.program)}
			for &place in bloc_for_insert.labels_to_be_offset {
				bloc_for_insert.program[place.upper].args[place.inner].i += offset
				place.upper += offset
			}
			for &unres in bloc_for_insert.labels_still_unresolved {
				unres.place.upper += offset
			}
			append(&block.program, ..bloc_for_insert.program[:])
			append(&block.labels_still_unresolved, ..bloc_for_insert.labels_still_unresolved[:])
			append(&block.labels_to_be_offset, ..bloc_for_insert.labels_to_be_offset[:])
		case .I_SYSCALL:
			right_type := type_of_valthing(statement.right[1], scope)
			add_MOV_2R_tac(1, right_type, statement.right[1], &block, &var_places, scope)

			arity := len(statement.right)
			if arity > 2 {
				add_tac(&block, .PUSH, Word{i = arity - 2})
				add_tac(&block, .MOV_I2R, Word{i = 2}, Word{sint_8B = i64(stack_offsets.frame_size)})
				offset_to_args := stack_offsets.frame_size
				for j in 2..<arity {
					right_type := type_of_valthing(statement.right[j], scope)
					add_MOV_2M_tac(offset_to_args + 8*(j - 2), right_type, statement.right[j], &block, &var_places, scope)
				}
			}
			add_tac(&block, .SYSCALL_0 + Op_code(arity - 2), Word{i = 0}, Word{i = 1}, Word{i = 2})
			if arity > 2 {
				add_tac(&block, .POP, Word{i = arity - 2})
			}
			left := retrieve_var_place(statement.left[0], &var_places)
			add_MOV_R2M_tac(left.place, left.type, 0, &block)
			
		} /* closes instruction switch statement */
		case ^Scope: panic("No idea how to deal with scope in a codegen statement")
		} /* closes switch statement that goes over the first thing in a statement */

		if long_assignment {
			for j in 0..<REGISTER_AMOUNT {
				if j >= len(statement.left) do break
				left := retrieve_var_place(statement.left[j], &var_places)
				add_MOV_2R_tac(j, left.type, statement.right[j], &block, &var_places, scope)
			}
			if len(statement.left) > 4 {
				/* OPTIM: Make this allocate in a more packed way */
				add_tac(&block, .PUSH, Word{i = len(statement.left) - 4})
				for j in 4..<len(statement.left) {
					left_type := retrieve_var_place(statement.left[j], &var_places).type
					add_MOV_2M_tac(stack_offsets.frame_size + 8*(j - 4), left_type, statement.right[j], &block, &var_places, scope)
				}
				for j in 4..<len(statement.left) {
					left := retrieve_var_place(statement.left[j], &var_places)
					add_MOV_M2M_tac(left.place, left.type, stack_offsets.frame_size + 8*(j - 4), &block)
				}
				add_tac(&block, .POP, Word{i = len(statement.left) - 4})
			}
			for j in 0..<REGISTER_AMOUNT {
				if j >= len(statement.left) do break
				left := retrieve_var_place(statement.left[j], &var_places)
				add_MOV_R2M_tac(left.place, left.type, j, &block)
			}
		}/* closes long_assignment */
	}/* closes gen_loop */

	if scope.kind == .PROC {
		caller_mem := stack_offsets.vars_1B/8
		add_tac(&block, .POP, Word{i = stack_offsets.frame_size/8 - caller_mem})
		add_tac(&block, .DYN_JUMP, Word{i = 8})
	} else if scope.kind == .GLOB {
		add_tac(&block, .DONE)
	}
	for name, proc_block in procs {
		add_tac(&block, .JUMP, Word{/* is filled out in 2 lines */})
		offset := len(block.program)
		block_infos[name] = {offset, len(proc_block.program)}
		block.program[offset-1].args[0].i = offset + len(proc_block.program)
		for &place in proc_block.labels_to_be_offset {
			proc_block.program[place.upper].args[place.inner].i += offset
			place.upper += offset
		}
		for &unres in proc_block.labels_still_unresolved {
			unres.place.upper += offset
		}
		append(&block.program, ..proc_block.program[:])
		append(&block.labels_still_unresolved, ..proc_block.labels_still_unresolved[:])
		append(&block.labels_to_be_offset, ..proc_block.labels_to_be_offset[:])
	}
	new_unresolved: [dynamic]Unresolved_name
	for unres in block.labels_still_unresolved {
		info, exists := block_infos[unres.name]
		if !exists {
			append(&new_unresolved, unres)
			continue
		}
		resolve := info.place
		if unres.skip_inst {
			resolve += info.length
		}
		block.program[unres.place.upper].args[unres.place.inner].i = resolve
		append(&block.labels_to_be_offset, unres.place)
	}
	delete(block.labels_still_unresolved)
	block.labels_still_unresolved = new_unresolved

//	println(block.labels_still_unresolved)
//	println(block.labels_to_be_offset)
//	println("---------------------------------------------------------------------------")
//	print_program(block)
	return block
}

add_MOV_R2R_tac :: proc(left: int, left_type, right: int, block: ^Block) {
	assert(left < REGISTER_AMOUNT)
	assert(right < REGISTER_AMOUNT)
	op_code := Op_code.MOV_1B_R2R + log2[size_of_type[left_type]]
	add_tac(block, op_code, Word{i = left}, Word{i = right})
}

add_MOV_R2M_tac :: proc(left: int, left_type: Type, right: int, block: ^Block) {
	assert(right < REGISTER_AMOUNT)
	op_code := Op_code.MOV_1B_R2M + log2[size_of_type[left_type]]
	add_tac(block, op_code, Word{i = left}, Word{i = right})
}

add_MOV_M2R_tac :: proc(left: int, left_type: Type, right: int, block: ^Block) {
	assert(left < REGISTER_AMOUNT)
	op_code := Op_code.MOV_1B_M2R + log2[size_of_type[left_type]]
	add_tac(block, op_code, Word{i = left}, Word{i = right})
}

add_MOV_M2M_tac :: proc(left: int, left_type: Type, right: int, block: ^Block) {
	op_code := Op_code.MOV_1B_M2M + log2[size_of_type[left_type]]
	add_tac(block, op_code, Word{i = left}, Word{i = right})
}

add_MOV_2R_tac :: proc(reg: int, left: Type, right: Valthing, block: ^Block, var_places: ^Var_places, scope: ^Scope){
	assert(reg < REGISTER_AMOUNT)
	op_code: Op_code
	right_word: Word
	if val, is_value := right.(Value); is_value {
		op_code = .MOV_I2R
		if left == .T_F32 {
			right_word = Word{float_4B = f32(val.(f64))}
		} else if left == .T_F64 || left == .T_FLOAT {
			right_word = Word{float_8B = val.(f64)}
		} else do right_word = Word{sint_8B = i64(val.(int))}
	} else if name, is_name := right.(Name); is_name {
		decf, _ := visible_decfined(scope, name)
		if decf.is_variable {
			right_var := retrieve_var_place(name, var_places)
			op_code = .MOV_1B_M2R + log2[size_of_type[right_var.type]]
			right_word = Word{i = right_var.place}
		} else {
			op_code = Op_code.MOV_I2R
			if left == .T_F32 {
				right_word = Word{float_4B = f32(decf.content.(Value).(f64))}
			} else if left == .T_F64 || left == .T_FLOAT {
				right_word = Word{float_8B = decf.content.(Value).(f64)}
			} else do right_word = Word{sint_8B = i64(decf.content.(Value).(int))}
		}
	} else do panic("expected value or name")
	add_tac(block, op_code, Word{i = reg}, right_word)
}

add_MOV_2M_tac :: proc(left: int, left_type: Type, right: Valthing, block: ^Block, var_places: ^Var_places, scope: ^Scope){
	op_code: Op_code
	right_word: Word
	if val, is_value := right.(Value); is_value {
		op_code = .MOV_1B_I2M + log2[size_of_type[left_type]]
		if left_type == .T_F32 {
			right_word = Word{float_4B = f32(val.(f64))}
		} else if left_type == .T_F64 || left_type == .T_FLOAT {
			right_word = Word{float_8B = val.(f64)}
		} else do right_word = Word{sint_8B = i64(val.(int))}
	} else if name, is_name := right.(Name); is_name {
		decf, _ := visible_decfined(scope, name)
		if decf.is_variable {
			right_var := retrieve_var_place(name, var_places)
			op_code = .MOV_1B_M2M + log2[size_of_type[right_var.type]]
			right_word = Word{i = right_var.place}
		} else {
			op_code = Op_code.MOV_1B_I2M + log2[size_of_type[left_type]]
			if left_type == .T_F32 {
				right_word = Word{float_4B = f32(decf.content.(Value).(f64))}
			} else if left_type == .T_F64 || left_type == .T_FLOAT {
				right_word = Word{float_8B = decf.content.(Value).(f64)}
			} else do right_word = Word{sint_8B = i64(decf.content.(Value).(int))}
		}
	} else do panic("expected value or name")
	add_tac(block, op_code, Word{i = left}, right_word)
}

retrieve_var_place :: proc(name: Name, var_places: ^Var_places) -> Var_info {
	info, exists := var_places.vars[name]
	if !exists {
		assert(var_places.scope_above != nil, "Undeclared variable in codegen")
		info = retrieve_var_place(name, var_places.scope_above)
	}
	return info
}

add_tac :: proc(block: ^Block, op_code: Op_code, a0 := Word{}, a1 := Word{}, a2 := Word{}) {
	tac := Tac{
		op_code,
		{a0, a1, a2}
	}
	append(&block.program, tac)
}

collect_variables :: proc(scope: ^Scope, amounts: ^Var_ams, do_recurse: bool) {
	for name in scope.names_inbody {
		decf := scope.decfineds[name]
		switch decf.type {
			case .T_UNKNOWN: panic("Unknown type in collect_variables")
			case .T_BYTE, .T_U8, .T_S8: amounts.vars_1B += 1
			case .T_U16, .T_S16: amounts.vars_2B += 1
			case .T_U32, .T_S32, .T_F32: amounts.vars_4B += 1
			case .T_U64, .T_S64, .T_F64, .T_PTR, .T_WORD: amounts.vars_8B += 1
			case .T_TYPE, .T_INSTCN: panic("Type and Instructions aren't really welldefined types yet")
			case .T_PROC: /* Ignore these, since they don't take up space on the stack. */
			case .T_BLOC: if do_recurse do collect_variables(decf.content.(^Scope), amounts, true)
			case .T_INT, .T_FLOAT, .T_NAME: assert(!decf.is_variable, "T_INT, T_FLOAT, and T_NAME should not be in variables")
		}
	}
}

print_program :: proc(block: Block) {
	for tac, i in block.program {
		printf("0x%x | %v 0x%x 0x%x 0x%x\n", i, tac.op_code, tac.args[0].i, tac.args[1].i, tac.args[2].i)
	}
}

print_tac :: proc(tac: Tac) {
	printf("%v 0x%x 0x%x 0x%x\n",  tac.op_code, tac.args[0].i, tac.args[1].i, tac.args[2].i)
}