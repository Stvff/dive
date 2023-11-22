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

print_help :: proc() {
	printf(
`dive: A somewhat properly typed and procedural programming language.
	usage: dive <arguments> path/to/entry/file.dive
	arguments:
		-check
		-debug-tokens
		-debug-parse
		-debug-gen
		-debug-run
		-help

For questions or bugs, go to https://www.github.com/StevenClifford/dive or email steve2020dev@gmail.com.
`)
}

debug_tokens, debug_parse, debug_gen, debug_tac, debug_run: bool

main :: proc(){
	program_name: string
	if len(os.args) == 1 {
		println("dive: did not get any arguments.")
		print_help()
		return
	}

	only_check: bool
	for i in 1..<len(os.args) {
		if os.args[i][0] == '-' {
			switch os.args[i][1:] {
			case "debug-tokens": debug_tokens = true
			case "debug-parse": debug_parse = true
			case "debug-gen": debug_gen = true
			case "debug-tac": debug_tac = true
			case "debug-run": debug_run = true
			case "check": only_check = true
			case "help": print_help()
			case:
				printf("dive: `%v` is not a recognized argument.\n", os.args[i])
				print_help()
			}
			continue
		}
		program_name = os.args[i]
	}

	scope, err := parse_and_check(program_name)
	if err do return

	if debug_gen do println("---------------------------------------------------------------------------")
	block := generate_bytecode(scope, nil, nil)
	if debug_gen do println("---------------------------------------------------------------------------")
	if debug_tac {
		println("---------------------------------------------------------------------------")
		print_program(block)
		println("---------------------------------------------------------------------------")
	}
	if only_check do return

	run(block.program[:], debug_run)
}

parse_and_check :: proc(program_name: string) -> (^Scope, bool) {
	prograwdata, file_success := os.read_entire_file(program_name)
	if !file_success {
		printf("dive: invalid file `%v` as input.\n", program_name)
		return nil, true
	}
	program_string := cast(string)prograwdata

	entry_program_info := new(Program_string_info)
	entry_program_info ^= Program_string_info {
		program_name,
		program_string
	}

	tokens := tokenize(entry_program_info)
	if debug_tokens {
		println("---------------------------------------------------------------------------")
		for tok in tokens do println(tok)
		println("---------------------------------------------------------------------------")
	}
	defer delete(tokens)

	scope, ltok, parse_err := parse_scope(tokens[:], .GLOB)
	if parse_err do return scope, parse_err

	if debug_parse {
		println("---------------------------------------------------------------------------")
		print_scope("", scope, 0)
		println("---------------------------------------------------------------------------")
	}

	if check(scope) do return scope, true

	return scope, false
}


/* IDEA: Write to some buffer instead of stderr immediately, so that people can
   limit the amount of errors that gets thrown at their face. */
print_error :: proc(message: string, error_poslen: Poslen, there_will_be_more := false) {
	program_string := error_poslen.info.body
	program_name := error_poslen.info.name
	error_pos := error_poslen.pos
	error_len := error_poslen.len

	fmt.eprintf("in file `%v`: %v\n", program_name, message)
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

print_statement :: proc(statement: Statement, scope: ^Scope, depth: int) {
	pf :: fmt.printf
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
//				for i in 0..<depth do pf("\b"); pf(" ")
//				print_scope(namae, scope.decfineds[namae].content.(^Scope), depth + 1)
//				do_semicolon = false
				pf("%v :: <scope> ", namae)
				break right_loop
			} else do pf("%v ", instruction_strings[q - Instruction(1)])
		case ^Scope: print_scope("", scope, depth + 1)
		case: pf("INVALID TOKEN")
	}
	if do_semicolon do pf("\b;\n")
}