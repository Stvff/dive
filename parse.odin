package dive

add_statement_to_decfined :: proc(decfineds: ^map[Name]Decfined, statement: Statement) -> (err := false) {
	if statement.kind == .DECL { for i in 0..<min(len(statement.left), len(statement.right)) {
		name := statement.left[i]
		if d, already_exists := decfineds[name]; already_exists {
			print_error("Attempted redeclaration of a variable that already exists in this scope:", statement.dposlens[i], true)
			print_error("Variable previously defined here:", {d.dpos, len(name)})
			err = true
			continue
		}
		decf: Decfined
		decf.dpos = statement.dposlens[i].pos
		decf.type = statement.right[i].(Type)
		decf.is_variable = true
		decfineds[name] = decf
	}} else if statement.kind == .DEFI { for i in 0..<min(len(statement.left), len(statement.right)){
		name := statement.left[i]
		if d, already_exists := decfineds[name]; already_exists {
			print_error("Attempted redefinition of a constant that already exists in this scope:", statement.dposlens[i], true)
			print_error("Variable previously defined here:", {d.dpos, len(name)})
			err = true
			continue
		}
		decf: Decfined
		decf.dpos = statement.dposlens[i].pos
		decf.content = statement.right[i]
		switch q in statement.right[i] {
			case Value:
				if  _, is_float := q.(f64); is_float do decf.type = .T_FLOAT
				else if _, is_int := q.(int); is_int do decf.type = .T_INT
				else do panic("Value unaccounted for in constant definition")
			case Name: decf.type = .T_UNKNOWN
			case Type: decf.type = .T_TYPE
			case Instruction: decf.type = .T_INSTCN
			case ^Scope:
				if q.kind == .PROC do decf.type = .T_PROC
				else if q.kind == .BLOC do decf.type = .T_BLOC
				else do decf.type = .T_BLOC
		}
		decf.is_variable = false
		decfineds[name] = decf
	}}
	return err
}

parse_scope :: proc(tokens: []Token, scope_kind: Scope_kind) -> (^Scope, []Token, bool) {
	tokens := tokens
	scope := new(Scope)
	scope.kind = scope_kind
	err := false
	statement: Statement
	if scope_kind == .PROC {
		k: Keyword; is_k: bool
		for len(tokens) > 0 {
			terr := false
			defer if terr do err = terr
			k, is_k = tokens[0].t.(Keyword)
			if is_k && (k == .K_ARG_SEPERATOR || k == .K_PAREN_CLOSE) {
				tokens = tokens[1:]
				break
			}
			statement, tokens, terr = parse_statement(tokens)
			if statement.kind != .DECL && len(statement.dposlens) > 0 {
				poslen := statement.dposlens[len(statement.left)]
				print_error("Procedure decleration can only contain variable declerations.", poslen)
				terr = true
				continue
			}
			append(&scope.parameters_input, ..statement.left[:])
			if add_statement_to_decfined(&scope.decfineds, statement) do terr = true
		}
		if is_k && (k == .K_ARG_SEPERATOR) {
			for len(tokens) > 0 {
				terr := false
				defer if terr do err = terr
				k, is_k = tokens[0].t.(Keyword)
				if is_k && k == .K_PAREN_CLOSE {
					tokens = tokens[1:]
					break
				}
				statement, tokens, terr = parse_statement(tokens)
				if statement.kind != .DECL && len(statement.dposlens) > 0 {
					poslen := statement.dposlens[len(statement.left)]
					print_error("Procedure decleration can only contain variable declerations.", poslen)
					terr = true
					continue
				}
				append(&scope.parameters_output, ..statement.left[:])
				if add_statement_to_decfined(&scope.decfineds, statement) do terr = true
			}
		}
		if k, is_k := tokens[0].t.(Keyword); !is_k || k != .K_BRACE_OPEN {
			print_error("Missing opening brace for procedure body.", tokens[0].poslen)
			err = true
		} else do tokens = tokens[1:]
	}

	for len(tokens) > 0 {
		terr := false
		defer if terr do err = terr
		k, is_k := tokens[0].t.(Keyword)
		if is_k && k == .K_BRACE_CLOSE {
			break
		}
		statement, tokens, terr = parse_statement(tokens)
		if statement.kind == .ASSI || statement.kind == .BARE do append(&scope.statements, statement)
		else {
			for i in 0..<min(len(statement.left), len(statement.right)) {
				just_defd_scope, is_just_defd_scope := statement.right[i].(^Scope)
				if is_just_defd_scope {
					just_defd_scope.scope_above = scope
					statement.right[i] = just_defd_scope
				if just_defd_scope.kind == .BLOC {
					label_statement: Statement
					label_statement.kind = .BARE
					append(&label_statement.right, Instruction.I_LABEL)
					append(&label_statement.right, statement.left[i])
					append(&label_statement.dposlens, statement.dposlens[i], statement.dposlens[i])
					append(&scope.statements, label_statement)
				}}
			}
			append(&scope.names_inbody, ..statement.left[:])
			if add_statement_to_decfined(&scope.decfineds, statement) do terr = true
		}
	}
	return scope, tokens, err
}

parse_statement :: proc(tokens: []Token) -> (Statement, []Token, bool) {
	tokens := tokens
	statement := Statement{kind = .BARE}
	progress: enum{ LEFT, RIGHT }
	err := false
	main_loop: for len(tokens)>0 {
		token := tokens[0]
		keyword, is_keyword := token.t.(Keyword)
		if keyword == .K_SEMICOLON {
			tokens = tokens[1:]
			break main_loop
		}
		if keyword == .K_BRACE_CLOSE {
			is_scope: bool
			for s in statement.right do if _, is_scope = s.(^Scope); is_scope do break
			if !is_scope {
				print_error("Missing semicolon at end of scope.", token.poslen)
				err = true
			}
			tokens = tokens[1:]
			break main_loop
		}
		if k := keyword; k == .K_ARG_SEPERATOR || k == .K_PAREN_CLOSE || k == .K_BRACKET_CLOSE {
			break main_loop
		}
		tokens = tokens[1:]
		switch progress {
		case .LEFT:
		poslen := token.poslen
		append(&statement.dposlens, poslen)
		sss, is_str := token.t.(string)
		name := cast(Name) sss
		if !is_str {
			progress = .RIGHT
			if is_keyword do #partial switch keyword {
				case .K_EQUAL:
					statement.kind = .ASSI
					continue
				case .K_COLON:
					statement.kind = .DECL
					continue
				case .K_DEFINE:
					statement.kind = .DEFI
					continue
			}
			statement.kind = .BARE
			is_instr, instruction := is_instruction(token)
			if !is_instr {
				print_error("Left side of a statement must only contain identifiers.", token.poslen)
				return statement, tokens, true
			}
			append(&statement.right, cast(Valthing) instruction)
			continue
		}
		append(&statement.left, name)

		case .RIGHT:
		poslen := token.poslen
		append(&statement.dposlens, poslen)
		switch statement.kind {
			case .BARE: fallthrough
			case .ASSI:
				is_instr, instruction := is_instruction(token)
				name, is_str := token.t.(string)
				integer, is_integer := token.t.(int)
				float, is_float := token.t.(f64)
				switch {
					case is_instr: append(&statement.right, cast(Valthing) instruction)
					case is_str: append(&statement.right, cast(Valthing) cast(Name) name)
					case is_integer: append(&statement.right, cast(Valthing) cast(Value) integer)
					case is_float: append(&statement.right, cast(Valthing) cast(Value) float)
					case: 
						print_error("Right side of an assignment can only contain instructions, identifiers, integers or floats.", token.poslen)
						return statement, tokens, true
				}
			case .DECL:
				is_type, type := is_base_type(Token{t = keyword})
				if !is_type {
					print_error("Right side of a decleration must only contain types.", token.poslen)
					return statement, tokens, true
				}
				append(&statement.right, cast(Valthing) type)
			case .DEFI:
				keyword, is_keyword := token.t.(Keyword)
				integer, is_integer := token.t.(int)
				float, is_float := token.t.(f64)
				switch {
					case is_keyword:
						if keyword == .K_PAREN_OPEN || keyword == .K_BRACE_OPEN {
							scope: ^Scope; leftovers: []Token; terr := false
							if keyword == .K_PAREN_OPEN {
								scope, leftovers, terr = parse_scope(tokens, .PROC)
							} else {
								scope, leftovers, terr = parse_scope(tokens, .BLOC)
							}
							tokens = leftovers
							append(&statement.right, scope)
							if terr do err = true
						} else {
							print_error("Right side of a definition can only contain integers, floats and function or block definitions.", token.poslen)
							return statement, tokens, true
						}
					case is_integer: append(&statement.right, cast(Valthing) cast(Value) integer)
					case is_float: append(&statement.right, cast(Valthing) cast(Value) float)
					case:
						print_error("Right side of a definition can only contain integers, floats and function or block definitions.", token.poslen)
						return statement, tokens, true
				}
		}
	}}
	return statement, tokens, err
}

is_instruction :: proc(token: Token) -> (bool, Instruction) {
	keyword, is_keyword := token.t.(Keyword)
	if !is_keyword do return false, nil
	if !(.K_INSTRUCTIONS_START < keyword && keyword < .K_INSTRUCTIONS_END) do return false, nil
	i := Instruction(keyword - .K_INSTRUCTIONS_START)
	return true, i
}

is_base_type :: proc(token: Token) -> (bool, Base_type) {
	keyword, is_keyword := token.t.(Keyword)
	if !is_keyword do return false, nil
	if !(.K_TYPES_START < keyword && keyword < .K_TYPES_END) do return false, nil
	b := Base_type(keyword - .K_TYPES_START)
	return true, b
}

import "core:strconv"
/* SPEED: this function contains two loops, and I believe they can be combined into one loop, since
   both loops only ever look backwards */
tokenize :: proc(program_string: string) -> [dynamic]Token {
	Nibbit :: struct{
		str: string,
		pos: int
	}
	nibbits: [dynamic]Nibbit
	defer delete(nibbits); {
		start := 0
		was_whitespace := true
		for char, i in program_string {
			if is_in(char, stoptokens) {
				if !was_whitespace {
					append(&nibbits, Nibbit{program_string[start:i], start})
				}
				append(&nibbits, Nibbit{program_string[i:i+1], i})
				start = i+1
				was_whitespace = true
				continue
			}
			is_whitespace := is_in(char, whitespace)
			if !is_whitespace && was_whitespace {
				start = i
			}
			if is_whitespace && !was_whitespace {
				append(&nibbits, Nibbit{program_string[start:i], start})
			}
			was_whitespace = is_whitespace
		}
	}

	tokens: [dynamic]Token
	prev_token: Token
	for i := 0; i < len(nibbits); i += 1 {
		nib := nibbits[i]
		keyword, is_keyword := keywords[nib.str]
		token := Token{poslen = {nib.pos, len(nib.str)}}
		if is_keyword {
			if keyword == .K_COLON && prev_token.t == .K_COLON {
				tokens[len(tokens)-1] = {
					{prev_token.poslen.pos, token.poslen.pos + token.poslen.len - prev_token.poslen.pos},
					Keyword.K_DEFINE
				}
				prev_token = {}
				continue
			}
			if keyword == .K_HYPHEN && prev_token.t == .K_HYPHEN {
				tokens[len(tokens)-1] = {
					{prev_token.poslen.pos, token.poslen.pos + token.poslen.len - prev_token.poslen.pos},
					Keyword.K_ARG_SEPERATOR
				}
				prev_token = {}
				continue
			}
			if keyword == .K_COMMA do continue
			token.t = keyword
		} else if n, is_int := strconv.parse_int(nib.str); is_int {
			if prev_token.t == .K_HYPHEN {
				tokens[len(tokens)-1] = {
					{prev_token.poslen.pos, token.poslen.pos + token.poslen.len - prev_token.poslen.pos},
					-n
				}
				prev_token = {}
				continue
			}
			token.t = n
		} else if f, is_float := strconv.parse_f64(nib.str); is_float {
			if prev_token.t == .K_HYPHEN {
				tokens[len(tokens)-1] = {
					{prev_token.poslen.pos, token.poslen.pos + token.poslen.len - prev_token.poslen.pos},
					-f
				}
				prev_token = {}
				continue
			}
			token.t = f
		} else {
			token.t = nib.str
		}
		prev_token = {token.poslen, keyword} if is_keyword else {}
		append(&tokens, token)
	}
	return tokens
}
