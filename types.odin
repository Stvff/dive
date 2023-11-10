package dive

/* <Bytecode> */
REGISTER_AMOUNT :: 4
STACK_SIZE :: 20_000

Tac :: struct {
	op_code: Op_code,
	args: [3]Word
}

Op_code :: enum i64 {ERR,
	MOV_1B_R2R, MOV_2B_R2R, MOV_4B_R2R, MOV_8B_R2R,
	MOV_1B_R2M, MOV_2B_R2M, MOV_4B_R2M, MOV_8B_R2M,
	MOV_1B_M2R, MOV_2B_M2R, MOV_4B_M2R, MOV_8B_M2R,
	MOV_1B_M2M, MOV_2B_M2M, MOV_4B_M2M, MOV_8B_M2M,

	MOV_I2R,
	MOV_1B_I2M, MOV_2B_I2M, MOV_4B_I2M, MOV_8B_I2M,

	CAST_F4_TO_S8, CAST_S8_TO_F4,
	CAST_F8_TO_S8, CAST_S8_TO_F8,
	CAST_F4_TO_F8, CAST_F8_TO_F4,

	READ_1B, READ_2B, READ_4B, READ_8B,
	WRITE_1B, WRITE_2B, WRITE_4B, WRITE_8B,
	ADDR,

//	CALL, PUSH, RETURN, POST_CALL,
	WINDUP, PUSH, POP, REWIND,

	ADD_UINT, ADD_SINT, ADD_F32, ADD_F64,
	SUB_UINT, SUB_SINT, SUB_F32, SUB_F64,
	MUL_UINT, MUL_SINT, MUL_F32, MUL_F64,
	DIV_UINT, DIV_SINT, DIV_F32, DIV_F64,

	GROWS_UINT, GROWS_SINT, GROWS_F32, GROWS_F64,
	SHNKS_UINT, SHNKS_SINT, SHNKS_F32, SHNKS_F64,
	EQU_UINT, EQU_SINT, EQU_F32, EQU_F64,

	JUMP_IF, JUMP_IFN,
	JUMP, DYN_JUMP,

	SYSCALL_0, SYSCALL_1, SYSCALL_2, SYSCALL_3, SYSCALL_4, SYSCALL_5, SYSCALL_6,

	DONE,
}

Word :: struct #raw_union {
	i: int,

	uint_1B: u8,
	uint_2B: u16,
	uint_4B: u32,
	uint_8B: u64,

	sint_1B: i8,
	sint_2B: i16,
	sint_4B: i32,
	sint_8B: i64,

	float_4B: f32,
	float_8B: f64,

	ptr_8B: rawptr,
	uptr_8B: uintptr,

	one_byte: byte,
	two_byte: [2]byte,
	four_byte: [4]byte,
	eight_byte: [8]byte,

	four_two_byte: [4][2]byte,
	two_four_byte: [2][4]byte,
}
#assert(size_of(Word) == 8, "Size of word is not 8 bytes")

Var_ams :: struct {
	vars_1B, vars_2B, vars_4B, vars_8B: int,
	frame_size: int
}

Var_places :: struct {
	scope_above: ^Var_places `fmt:"-"`, /* this random string is for telling fmt.print* to ignore this */
	vars: map[Name]Var_info
}

Var_info :: struct {
	type: Type,
	place: int,
}

Block :: struct {
	program: [dynamic]Tac,
	labels_still_unresolved: [dynamic]Unresolved_name,
	labels_to_be_offset: [dynamic]Program_place
}

Unresolved_name :: struct{
	skip_inst: bool,
	name: Name,
	place: Program_place
}

Block_info :: struct{
	place, length: int
}

/* []tac{....}[i.upper].args[i.inner] */
Program_place :: struct {
	upper, inner: int
}

/* </Bytecode> */

Scope_kind :: enum{GLOB, PROC, BLOC}
Scope :: struct{
	kind: Scope_kind,
	scope_above: ^Scope `fmt:"-"`, /* this random string is for telling fmt.print* to ignore this */
	parameters_input: [dynamic]Name,
	parameters_output: [dynamic]Name,
	names_inbody: [dynamic]Name,
	decfineds: map[Name]Decfined,
	statements: [dynamic]Statement
}

Decfined :: struct{
	dpos: int,
	type: Type,
	is_variable: bool,
	content: Valthing
}

Statement_kind :: enum{ASSI, DECL, DEFI, BARE}
Statement :: struct{
	kind: Statement_kind,
	left: [dynamic]Name,
	right: [dynamic]Valthing,
	dposlens: [dynamic]Poslen
}

Valthing :: union{
	Value, Name, Type, Instruction, ^Scope
}

Name :: distinct string

/* IDEA: make this an array of `Base_type`s, to facilitate nested types. */
Type :: Base_type

Value :: union{
	int, f64
}

log2 := [?]Op_code{nil, /*1*/ Op_code(0), /*2*/ Op_code(1), nil, /*4*/ Op_code(2), nil, nil, nil, /*8*/ Op_code(3)}
size_of_type := [?]int{0,
	1, 1, 1, 2, 2, 4, 4, 8, 8,
	4, 8, 8, 8,
	8, 8, 8, 8,
	8, 8
}
Base_type :: enum{ /* synchronize this with Keyword enum */
	T_UNKNOWN,
	T_BYTE, T_U8, T_S8, T_U16, T_S16, T_U32, T_S32, T_U64, T_S64,
	T_F32, T_F64, T_PTR, T_WORD,
	T_TYPE, T_INSTCN,
	T_PROC, T_BLOC,
	T_INT, T_FLOAT, T_NAME
}

Instruction :: enum{ /* synchronize this with Keyword enum */
	I_UNKNOWN,
	I_CAST, I_TRANS, I_READ, I_WRITE, I_ADDR,
	I_CALL, I_RETURN,
	I_ADD, I_SUB, I_MUL, I_DIV,
	I_GROWS, I_SHNKS, I_EQU,
	I_IF, I_IFN, I_SKIP,
	I_SYSCALL, I_LABEL
}

/* <tokenizing> */
Token :: struct{
	poslen: Poslen,
	t: union{ Keyword, int, f64, string }
}
Poslen :: struct{
	pos, len: int
}
Keyword :: enum { /* synchronize this with the string arrays */
	K_ERROR,
	K_INSTRUCTIONS_START, /* synchronize this with Instruction enum */
		K_CAST, K_TRANS, K_READ, K_WRITE, K_ADDR,
		K_CALL, K_RETURN,
		K_ADD, K_SUB, K_MUL, K_DIV,
		K_GROWS, K_SHNKS, K_EQU,
		K_IF, K_IFN, K_SKIP,
		K_SYSCALL,
	K_INSTRUCTIONS_END,

	K_DEFINE, K_ARG_SEPERATOR,

	K_OPERATORS_START,
		K_COLON, K_EQUAL, K_PAREN_OPEN, K_PAREN_CLOSE, K_BRACE_OPEN, K_BRACE_CLOSE, K_HYPHEN,
		K_BRACKET_OPEN, K_BRACKET_CLOSE, K_COMMA, K_SEMICOLON,
	K_OPERATORS_END,

	K_TYPES_START, /* synchronize this with Base_type enum */
		K_BYTE, K_U8, K_S8, K_U16, K_S16, K_U32, K_S32, K_U64, K_S64,
		K_F32, K_F64, K_PTR, K_WORD,
	K_TYPES_END,

	K_OTHER
}
instruction_strings := [?]string{
	"cast", "trans", "read", "write", "addr",
	"call", "return", "add",
	"sub", "mul", "div",
	"grows", "shnks", "equ",
	"if", "ifn", "skip",
	"syscall"
}
type_strings := [?]string{
	"byte", "u8", "s8", "u16", "s16",
	"u32", "s32", "u64", "s64",
	"f32", "f64", "ptr", "word"
}
stoptokens :: []rune {
	':', '=', '(', ')', '{', '}', '-',
	'[', ']',
	',', ';'
}

keywords: map[string]Keyword

import "core:unicode/utf8"
@(private) @(init) init_keywords :: proc() { 
	k := Keyword.K_INSTRUCTIONS_START + Keyword(1)
	for str in instruction_strings {
		keywords[str] = k
		k += Keyword(1)
		assert(k <= .K_INSTRUCTIONS_END)
	}

	keywords["::"] = .K_DEFINE
	keywords["--"] = .K_ARG_SEPERATOR

	k = Keyword.K_OPERATORS_START + Keyword(1)
	for tok in stoptokens {
		s := utf8.runes_to_string({tok})
		keywords[s] = k
		k += Keyword(1)
		assert(k <= .K_OPERATORS_END)
	}

	k = Keyword.K_TYPES_START + Keyword(1)
	for str in type_strings {
		keywords[str] = k
		k += Keyword(1)
		assert(k <= .K_TYPES_END)
	}
}
whitespace :: []rune {
	' ', '\t', '\v', '\n'
}
/* SPEED: make this a not a linear search */
is_in :: proc(char: rune, list: []rune) -> bool {
	for option, i in list do if option == char do return true
	return false
}
/* </tokenizing> */
