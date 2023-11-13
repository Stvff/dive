package dive

/* IDEA: Maybe we should also infer types of value literals here too.
         Type inference could also happen here in the exact same way,
   but there might be side effects and crossreference checking that I
   haven't thought of yet. I should write a big program where I do it
   by hand before I decide for sure. */
/* SPEED: add an `already checked` boolean to `Scope` to prevent us
   from checking an n-depth import n times */
check :: proc(scope: ^Scope) -> bool {
	if scope.already_checked do return false
	final_err := false
	for name, thing in scope.decfineds {
		if thing.type == .T_PROC || thing.type == .T_BLOC {
			err := check(thing.content.(^Scope))
			if err do final_err = true
		}
	}
	for statement in scope.statements {
		err := false
		left_types := make([dynamic]Type, 0, context.temp_allocator)
		defer delete(left_types)
		defer if err do final_err = true
		for name, i in statement.left {
			decf, exists := visible_decfined(scope, name)
			if !exists {
				print_error("Undeclared variable:", statement.dposlens[i])
				err = true
			} else if !decf.is_variable {
				print_error("Identifier on the left of the statement is not a variable:", statement.dposlens[i])
				err = true
			}
			append(&left_types, decf.type)
		}
		if !err {
			left_poslens := statement.dposlens[0:len(statement.left)]
			right_poslens := statement.dposlens[len(statement.left) + (statement.kind != .BARE ? 1 : 0):]
			if check_expression(scope, left_types[:], statement.right[:], left_poslens, right_poslens) do err = true
		}
	}
	scope.already_checked = true
	return final_err
}

check_expression :: proc(scope: ^Scope, desired: []Type, expression: []Valthing, left_poslens, right_poslens: []Poslen) -> bool {
	desired := desired; left_poslens := left_poslens; right_poslens := right_poslens; expression := expression
	if len(expression) != len(right_poslens) {
		println(expression, right_poslens)
		panic("expression and right_poslens mismatched")
	}
	if len(desired) != len(left_poslens) {
		println(desired, left_poslens)
		panic("desired and left_poslens mismatched")
	}
	err := false
	left_index, right_index: int
	main_loop: for len(expression) > 0 {
		if err do break
		defer {
			if len(desired) != 0 {
				desired = desired[1:]
				left_poslens = left_poslens[1:]
			}
			expression = expression[1:]
			right_poslens = right_poslens[1:]
		}
		/* FIXME: All these errors need to be much much clearer,
		   and actually print out the types that were expected.
		   Ideally, we'd make a print_type_error function, with
		   extra functionality. */
		switch q in expression[0] {
				case Value:
				if len(desired) == 0 {
					print_error("Value literal found, where no values were expected.", right_poslens[0])
					err = true
					continue main_loop
				}
				t := desired[0]
				if (.T_BYTE <= t && t <= .T_S64) || t == .T_INT || t == .T_PTR {
					_, is_int := q.(int)
					if !is_int {
						print_error("Integer literal was expected, but not given.", left_poslens[0], true)
						print_error("This value literal does not represent an integer:", right_poslens[0])
						err = true
					}
				} else if t == .T_F32 || t == .T_F64 || t == .T_FLOAT {
					_, is_float := q.(f64)
					if !is_float {
						print_error("Float literal was expected, but not given.", left_poslens[0], true)
						print_error("This value literal does not represent a float:", right_poslens[0])
						err = true
					}
				} else {
					print_error("The expected value accepts no literals.", left_poslens[0], true)
					print_error("The value literal:", right_poslens[0])
					err = true
				}
			case Name:
				if len(desired) == 0 {
					print_error("Identifier found, where no values were expected.", right_poslens[0])
					err = true
					continue main_loop
				}
				decf, exists := visible_decfined(scope, q)
				if !exists {
					print_error("Undeclared identifier:", right_poslens[0])
					err = true
					continue main_loop
				}
				t := desired[0]
				if (.T_BYTE <= decf.type && decf.type <= .T_S64) || decf.type == .T_INT || decf.type == .T_PTR {
					if !(.T_BYTE <= t && t <= .T_S64) && t != .T_INT && t != .T_PTR {
						print_error("Floating point value was expected, but not given.", left_poslens[0], true)
						print_error("This constant does not contain a floating point value:", right_poslens[0])
						err = true
					}
				} else if decf.type == .T_FLOAT || decf.type == .T_F32 || decf.type == .T_F64 {
					if t != .T_F32 && t != .T_F64 && t != .T_FLOAT {
						print_error("Integer value was expected, but not given.", left_poslens[0], true)
						print_error("This constant does not contain an integer value:", right_poslens[0])
						err = true
					}
				} else if desired[0] != decf.type {
					print_error("Mismatched types.\nExpectation was set by:", left_poslens[0], true)
					print_error("and not fulfilled by:", right_poslens[0], true)
					print_error("which was defined here:", {decf.dpos, len(q), decf.infile})
					err = true
				}
			case Type:
				print_error("Type in expressions are not yet implemented.", right_poslens[0])
				err = true
			/* IDEA: Make cast, load, and addr take varargs. */
			case Instruction: switch q {
				case .I_UNKNOWN:
					print_error("Unknown instruction.", right_poslens[0])
					err = true
				case .I_CAST:
					/* IDEA: Limit what types can be cast to what other types. */
					if len(expression) != 2 || len(desired) != 1 {
						print_error("`cast` takes and produces 1 value.", right_poslens[0])
						err = true
						continue main_loop
					}
					expression = expression[1:]
					right_poslens = right_poslens[1:]
				case .I_TRANS:
					if len(expression) != 2 || len(desired) != 1 {
						print_error("`trans` takes and produces 1 value.", right_poslens[0])
						err = true
						continue main_loop
					}
					expression = expression[1:]
					right_poslens = right_poslens[1:]
				case .I_READ:
					if len(expression) != 2 || len(desired) != 1 {
						print_error("`load` takes a pointer and produces a value.", right_poslens[0])
						err = true
						continue main_loop
					}
					if check_expression(scope, {.T_PTR}, expression[1:], right_poslens[0:1], right_poslens[1:]) {
						print_error("`load` takes a pointer and produces a value.", right_poslens[0])
						err = true
						continue main_loop
					}
					expression = expression[1:]
					right_poslens = right_poslens[1:]
				case .I_WRITE:
					if len(expression) != 3 || len(desired) != 0 {
						print_error("`write` takes a pointer and a variable, and produces no values.", right_poslens[0])
						err = true
						continue main_loop
					}
					name, is_name := expression[2].(Name)
					if !is_name {
						print_error("Since the size of variables is wellknown, `write` only takes variables, but it was not given a variable:", right_poslens[2])
						err = true
						continue main_loop
					}
					decf, exists := visible_decfined(scope, name)
					if !exists {
						print_error("Undeclared identifier:", right_poslens[1])
						err = true
						continue main_loop
					}
					if !decf.is_variable {
						print_error("Since the size of variables is wellknown, `write` only takes variables, but it was not given a variable:", right_poslens[2])
						err = true
						continue main_loop
					}
					if check_expression(scope, {.T_PTR, decf.type}, expression[1:], {right_poslens[0], right_poslens[0]}, right_poslens[1:]) {
						print_error("`write` takes a pointer and a variable, and produces no values.", right_poslens[0])
						err = true
						continue main_loop
					}
					expression = expression[2:]
					right_poslens = right_poslens[2:]
				case .I_ADDR:
					if len(expression) != 2 || len(desired) != 1 {
						print_error("`addr` takes a variable, and produces a pointer.", right_poslens[0])
						err = true
						continue main_loop
					}
					if desired[0] != .T_PTR {
						print_error("`addr` produces a pointer, but the variable on the left was not a pointer.", left_poslens[0])
						err = true
						continue main_loop
					}
					name, is_name := expression[1].(Name)
					if !is_name {
						print_error("`addr` only takes variables, since only those have an address, but was not given one.", right_poslens[1])
						err = true
						continue main_loop
					}
					decf, exists := visible_decfined(scope, name)
					if !exists {
						print_error("Undeclared identifier:", right_poslens[1])
						err = true
						continue main_loop
					}
					if !decf.is_variable {
						print_error("`addr` only takes variables, since only those have an address, but was given one.", right_poslens[1])
						err = true
						continue main_loop
					}
					expression = expression[1:]
					right_poslens = right_poslens[1:]
				case .I_CALL:
					if len(expression) < 2 {
						print_error("`call` takes a procedure name as its first argument, but was given no arguments.", right_poslens[0])
						err = true
						continue main_loop
					}
					proc_name, is_name := expression[1].(Name)
					if !is_name {
						print_error("`call` takes a procedure name as its first argument, but was not given a name:", right_poslens[1])
						err = true
						continue main_loop
					}
					decf, exists := visible_decfined(scope, proc_name)
					if !exists {
						print_error("`call` takes a procedure name as its first argument, but was given an undeclared name:", right_poslens[1])
						err = true
						continue main_loop
					}
					if decf.type != .T_PROC {
						print_error("`call` takes a procedure name as its first argument, but the name that was given was not for a procedure:", right_poslens[1])
						err = true
						continue main_loop
					}
					to_be_called := decf.content.(^Scope)
					if len(expression) != 2 + len(to_be_called.parameters_input) || len(desired) != len(to_be_called.parameters_output) {
						print_error("`call` takes and produces the same amount of values as the procedure it calls, but was not given that.", right_poslens[1], true)
						print_error("Procedure was defined here:", {decf.dpos, len(proc_name), decf.infile})
						err = true
						continue main_loop
					}
					for n, i in to_be_called.parameters_output {
						var_decf := to_be_called.decfineds[n]
						if desired[i] != var_decf.type {
							print_error("This variable is not the same type as the one expected by the procedure being called:", left_poslens[1], true)
							print_error("Expected type was defined here:", {var_decf.dpos, len(n), var_decf.infile})
							err = true
							continue main_loop
						}
					}
					procedure_input_types := make([dynamic]Type, 0, context.temp_allocator)
					procedure_input_poslens := make([dynamic]Poslen, 0, context.temp_allocator)
					defer delete(procedure_input_types)
					defer delete(procedure_input_poslens)
					for n, i in to_be_called.parameters_input {
						var_decf := to_be_called.decfineds[n]
						append(&procedure_input_types, var_decf.type)
						append(&procedure_input_poslens, Poslen{var_decf.dpos, len(n), var_decf.infile})
					}
					if check_expression(scope, procedure_input_types[:], expression[2:], procedure_input_poslens[:], right_poslens[2:]) {
						err = true
						continue main_loop
					}
					expression = expression[1 + len(to_be_called.parameters_input):]
					right_poslens = right_poslens[1 + len(to_be_called.parameters_input):]
					desired = desired[max(0, len(to_be_called.parameters_output) - 1):]
					left_poslens = left_poslens[max(0, len(to_be_called.parameters_output) - 1):]
				case .I_RETURN:
					/* FIXME: m = 1 return; does not give an error */
					if len(expression) != 1 || len(desired) != 0 {
						print_error("`ret` has no arguments and produces no values.", right_poslens[0])
						err = true
					}
				/* TODO: limit add, sub, compares, ifs, to the relevent types */
				case .I_ADD, .I_SUB, .I_MUL, .I_DIV:
					if len(expression) != 3 || len(desired) != 1 {
						print_error("Arithmetic instructions take 2 values and produce 1 value.", right_poslens[0])
						err = true
						continue main_loop
					}
					if check_expression(scope, {desired[0], desired[0]}, expression[1:], {right_poslens[0], right_poslens[0]}, right_poslens[1:]) {
						print_error("Arithmetic instructions take 2 arguments of the same type, which is given by the variable that the result is given to.", right_poslens[0])
						err = true
						continue main_loop
					}
					expression = expression[2:]
					right_poslens = right_poslens[2:]
				case .I_GROWS, .I_SHNKS, .I_EQU:
					if len(expression) != 3 || len(desired) != 1 {
						print_error("Comparison instructions take 2 values and produce 1 value.", right_poslens[0])
						err = true
						continue main_loop
					}
					expd_type := type_of_valthing(expression[1], scope)
					if check_expression(scope, {expd_type, expd_type}, expression[1:], {right_poslens[0], right_poslens[0]}, right_poslens[1:]) {
						print_error("Comparison instructions take 2 values of the same type.", right_poslens[0])
						err = true
						continue main_loop
					}
					expression = expression[2:]
					right_poslens = right_poslens[2:]
				case .I_IF, .I_IFN:
					if len(expression) != 3 || len(desired) != 0 {
						print_error("`if` and `ifn` take a value and a label and produce no value.", right_poslens[0])
						err = true
						continue main_loop
					}
					expd_type := type_of_valthing(expression[1], scope)
					if check_expression(scope, {expd_type, .T_BLOC}, expression[1:], {right_poslens[0], right_poslens[0]}, right_poslens[1:]) {
						print_error("`if` and `ifn` take a value and a label and produce no value.", right_poslens[0])
						err = true
						continue main_loop
					}
					expression = expression[2:]
					right_poslens = right_poslens[2:]
				case .I_SKIP:
					if len(expression) != 2 || len(desired) != 0 {
						print_error("`skip` takes a label and produces no values.", right_poslens[0])
						err = true
						continue main_loop
					}
					if check_expression(scope, {.T_BLOC}, expression[1:], {right_poslens[0]}, right_poslens[1:]) {
						print_error("`skip` only takes a label.", right_poslens[0])
						err = true
						continue main_loop
					}
					expression = expression[1:]
					right_poslens = right_poslens[1:]
				case .I_SYSCALL:
					if len(expression) < 2 || len(expression) > 8 || len(desired) != 1 {
						print_error("`syscall` takes anywhere from 1 to 6 arguments, and produces 1 value.", right_poslens[0])
						err = true
						continue main_loop
					}
					l := len(expression) - 1
					expression = expression[l:]
					right_poslens = right_poslens[l:]
				case .I_LABEL:
					expression = expression[1:]
					right_poslens = right_poslens[1:]
				}
			case ^Scope:
				print_error("Scope in expressions are not yet implemented.", right_poslens[0])
				err = true
		}
	}
	if !err && len(desired) > 0 {
		print_error("Expression does not produce enough values. All variables from here are unaccounted for:", left_poslens[0])
		err = true
	}
	return err
}

type_of_valthing :: proc(val: Valthing, scope: ^Scope) -> Type {
	switch q in val {
	case Value: switch v in q {
		case int: return .T_INT
		case f64: return .T_FLOAT
		case string: return .T_UNKNOWN;}
	case Name:
		decf, exists := visible_decfined(scope, q)
		if !exists do return .T_UNKNOWN
		return decf.type
	case Type: return .T_TYPE
	case Instruction: return .T_INSTCN
	case ^Scope: switch q.kind {
		case .PROC: return .T_PROC
		case .BLOC: return .T_BLOC
		case .GLOB: return .T_BLOC;}
	}
	return .T_UNKNOWN
}

visible_decfined :: proc(scope: ^Scope, name: Name, variables_allowed := true) -> (Decfined, bool) {
	variables_allowed := variables_allowed
	decf, exists := scope.decfineds[name]
	if !exists {
		/* IDEA: Variables in global scope might still be referenceable? Might make a `static` type modifier for this. */
		/* FIXME: Disallow labels from scopes above the procedure line. */
		if scope.scope_above != nil {
			if scope.kind == .PROC do variables_allowed = false
			decf, exists = visible_decfined(scope.scope_above, name, variables_allowed)
			if !variables_allowed && decf.is_variable do exists = false
		}
		else do exists = false
	}
	return decf, exists
}