package dive
import "core:fmt"
import "core:os"

println :: fmt.println
printf :: fmt.printf

/*
Comments to use in situations:
	FIXME: things that work for a specific scenario, but should work for more
	SPEED: optimization oppertunities
	TODO:  feature that has to be added
	IDEA:  feature that can be added, and/or makes sense to add
	CLEAN: messy definition/code that can be done better
	OPTIM: things that will make this compiler produce more optimal (byte)code

	Double slash comments (//) are for commenting out code, and similar temporary things,
	multi-line comments (/* */) are for information about the code.

	Commented out code should have the comment symbols on the first characters on the
	line, so not obeying the identation of the code.
	This helps seeing where code is commented out, for cleanup purposes.
*/

main :: proc(){
	prog_name: string
	if len(os.args) > 1 {
		prog_name = os.args[1]
	} else {
		println("dive: got no file as input")
		return
	}
	prograwdata, file_success :=  os.read_entire_file(prog_name)
	defer delete(prograwdata)
	if !file_success {
		println("dive: invalid file as input")
		return
	}
	program_string := cast(string)prograwdata
//	fmt.println(program_string)

	context.user_ptr = &program_string

	tokens := tokenize(program_string)
	defer delete(tokens)

	scope, ltok, parse_err := parse_scope(tokens[:], .GLOB)
	if parse_err do return
	if check(scope) do return

	print_scope("", scope, 0)
	println("---------------------------------------------------------------------------")

	block := generate_bytecode(scope, nil, nil)
	print_program(block)
	println("---------------------------------------------------------------------------")
	run(block.program[:])
}

/* IDEA: Write to some buffer instead of stderr immediately, so that people can
   limit the amount of errors that gets thrown at their face. */
print_error :: proc(message: string, error_poslen: Poslen, there_will_be_more := false) {
	program_string := (cast(^string) context.user_ptr)^
	error_pos := error_poslen.pos
	error_len := error_poslen.len

	fmt.eprintf("%v\n", message)
	line_nr, line_pos := 1, 0
	for i in 0..<error_pos {
		c := program_string[i]
		if c == '\n' {
			line_nr += 1
			line_pos = i + 1
		}
	}
	j := fmt.eprintf("%v | ", line_nr)
	line_len: int
	for i in line_pos..<len(program_string) {
		c := program_string[i]
		if c != '\n' do line_len += 1
		else do break
	}
	fmt.eprintf("%v\n", program_string[line_pos:line_pos + line_len])
	for q in 0..<j do fmt.eprintf(" ")
	for c in program_string[line_pos:error_pos] {
		if c == '\t' do fmt.eprintf("\t")
		else do fmt.eprintf(" ")
	}
	for q in 0..<error_len do fmt.eprintf("^")
	fmt.eprintf("\n")
	if !there_will_be_more do fmt.eprintf("\n")
}

/* FIXME: \t does not work properly in the terminal, and there is an indentation problem
          with nested scopes. */
print_scope :: proc(scope_name: Name, scope: ^Scope, depth: int) {
	depth := depth
	pf :: fmt.printf
	if scope.kind == .PROC {
		for i in 0..<depth - 2 do pf("\t")
		pf("%v :: (", scope_name)
		for name in scope.parameters_input {
			decf := scope.decfineds[name]
			pf("%v: %v; ", name, type_strings[decf.type - Type(1)])
		}
		if len(scope.parameters_output) > 0 do pf("-- ")
		for name in scope.parameters_output {
			decf := scope.decfineds[name]
			pf("%v: %v; ", name, type_strings[decf.type - Type(1)])
		}
		pf(") {{\n")
	} else if scope.kind == .BLOC {
		for i in 0..<depth - 2 do pf("\t")
		pf("%v :: {{\n", scope_name)	
	}
	for name in scope.names_inbody {
		decf := scope.decfineds[name]
		for i in 0..<depth do pf("\t")
		if decf.is_variable {
			pf("%v: %v;\n", name, type_strings[decf.type - Type(1)])
		}
		else if scope, is_scope := decf.content.(^Scope); is_scope {
			if scope.kind == .PROC do print_scope(name, scope, depth + 1)
			else do for i in 0..<depth do pf("\b")
		} else {
			pf("%v :: %v;\n", name, decf.content)
		}
	}
	for statement in scope.statements {
		for i in 0..<depth do pf("\t")
		do_semicolon := true
		if statement.kind == .ASSI {
			for v in statement.left do pf("%v ", v)
			pf("= ")
		}
		right_loop: for v, i in statement.right do switch q in v {
			case Value: pf("%v ", q)
			case Name: pf("%v ", q)
			case Type: pf("%v ", type_strings[q - Type(1)])
			case Instruction:
				if q == .I_LABEL {
					namae := statement.right[1].(Name)
					for i in 0..<depth do pf("\b"); pf(" ")
					print_scope(namae, scope.decfineds[namae].content.(^Scope), depth + 1)
					do_semicolon = false
					break right_loop
				} else do pf("%v ", instruction_strings[q - Instruction(1)])
			case ^Scope: print_scope("", scope, depth + 1)
			case: pf("INVALID TOKEN")
		}
		if do_semicolon do pf("\b;\n")
	}
	if scope.kind == .PROC || scope.kind == .BLOC {
		for i in 0..<depth-1 do pf("\t")
		pf("}\n")
	}
}