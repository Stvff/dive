# Dive

Dive is an imperative procedural language, that lives just on the edge of being a structured language, and compiles to a simple bytecode (with plans for more).
This means that instead of jumping to or calling labels, all code blocks in Dive are named and typechecked (so you can't jump to a procedure or call a basic block)
Furthermore, variables are statically and strongly typed with a basic typesystem. Procedure calls are pass-by-value.

As it is now, it is a very limited language, but I hope to add many things to bring it up to modern standards ([see the 'Todo' section](#to-do)).
```
import print.dive;

total: u64;
total = call factorial 10;
call println_u64 total;

factorial :: (n: u64 -- m: u64) {
	is_zero: u8;
	is_zero = equ n 0;
	if is_zero base_case;

	m = sub n 1;
	m = call factorial m;
	m = mul n m;
	return;

	base_case :: {
		m = 1;
		return;
	}
}
```

## Syntax and some semantics
All statements (except scope definitions) end with a semicolon. Comma's are whitespace and therefore optional.\
There are four types of statements: Definitions, declarations, assignments and bare expressions.

Definitions are done with a double colon `::`.
```
names :: literals;
```
`literals` can be integer or float literals, or a procedure or code block. Procedures and blocks are discussed later on.
Names that have been defined with this become constants, and are compiletime values.

Declarations are done with a single colon: `:`.
```
names: types;
```
The types are: `byte`, `u8`, `s8`, `u16`, `s16`, `u32`, `s32`, `u64`, `s64`, `f32`, `f64`, `ptr` and `word`. Pointers do not have a more specific type, and `word` is 8 bytes.\
Names that have been declared like this become variables, meaning that they exist and are mutable at runtime.

Assignments are done with `=`. Strictly speaking, there are three kinds of assignments: single assignment, multiple assignment and instruction expression.
An assignment always has variables on the left.
First, a single assignment is where there is only 1 variable on the left, and 1 value of the same type on the right.
In the context of statements, a 'Value' can be a variable, constant or integer or float literal.
```
a, b: s64, s64;
Q :: 4;
a = b;
a = Q;
a = 4;
```
Multiple assignments are assignments where there is more than one value on the right:
```
a, b = 10, 20;
a, b = b, a;
a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p = p, o, n, m, l, k, j, i, h, g, f, e, d, c, b, a;
```
These will swap the variables as expected, by copying to registers first, but if there are more than 4 variables, it starts copying them via the stack, so be aware.

The final type of assignment is one with an instruction expression.
Technically, instructions that don't produce a value (referred to earlier as 'bare expressions'), and therefore don't have an assignment to them, also fall under this catagory.
Instructions are in polish notation (so first the instruction, then the values), and can not be nested:
```
names = instruction values;
instruction values;
```
The [section 'Instructions'](#instructions) contains a table that goes over each instruction and what they do and need.

Lastly, there are two kinds of code blocks. Procedures, and basic blocks (or just 'blocks').
A basic block is a set of statements surrounded by curly braces, as in this example:
```
c: f32;
c = 1.1;
name :: {
	a, b: f32, f32;
	a = call some_procedure;
	b = mul a c;
}
```
New variables and definitions (including more blocks or procedures) can be made inside of a block, and it can refer to variables and definitions of the scopes above it,
however, variables and labels above the procedure scope that the block is defined in, can not be used. Instructions that use block labels are `if`, `ifn`, `skip_if` and `skip_ifn`.
When a program 'jumps' to a basic block, or when the program naturally enters it, it starts executing the instructions in the block, and when the end is reached,
it continues with the instructions that are defined after the block, and not the instructions after the jump. In other words, basic blocks don't return.

Procedures are code blocks that take and return arguments, and make their own stack frame.
They are defined as follows:
```
name :: (input_declarations -- output_declarations) {
	statements;
}
```
Both input and output parameters are named variables. A procedure can be prematurely exited with the `return` instruction. Unlike basic blocks, procedures do return to the callsite.\
The `call` instruction is used to call procedures. As its first argument, `call` takes the procedure name.
The arguments after that must match procedure's input parameters. Similarly, the variables that the `call` statement assigns to, must match the procedure's output parameters.

Inputs and outputs are seperated with a double hyphen `--`. When there are no outputs, the seperator is optional.
Some examples:
```
to_be_five: s64;
to_be_five = call returns_five;
returns_five :: (-- five: s64) {
	five = 5;
}
```
```
call doesnt 5 6;
doesnt :: (a: s64; b: s64) {
	a = mul a b;
}
```
As parameter declarations are declaration statements, they are seperated by semicolons.

Everything between angle brackets (`<` and `>`) is a comment, and comments can nest.

### Importing
Other files can be imported with the keyword `import`, followed by the path, and terminated by a semicolon.
```
import path/to/other/file.dive;
```
This will copy all the declarations and definitions in that file over to the current scope, with the exception of bare blocks and code at the global scope of the imported file.
Filepaths are relative to the file that the import is done in.
Currently, circular imports _will_ form a recursion loop, so be aware!

## To do
* bugfixes and a lot of testing
* more kinds of literals
* prefixes for `import`
* type, variable and instruction aliasing
* `:=` or some variation of it
* custom types
* nested expressions
* transpiling to assembly
* some way to interact with MS Windows

## Instructions

| name   | produces              | takes                        | description |
|--------|-----------------------|------------------------------|-------------|
|`cast`  | any type              | any type                     | converts any type to any other type |
|`trans` | any type              | any type                     | directly copies the bits from any type to any other type |
|`read`  | any type              | a pointer                    | reads a type-sized piece of memory at a pointer's location |
|`write` | nothing               | a pointer and a variable     | writes the value in the variable to the pointer's location |
|`addr`  | a pointer             | a variable                   | gives the address of a variable |
|`call`  | the procedure returns | a procedure name and the procedure parameters | calls a procedure |
|`return`| nothing               | nothing                      | exits a procedure |
|`add`   | a number              | two numbers of the same type | adds |
|`sub`   | a number              | two numbers of the same type | subtracts |
|`mul`   | a number              | two numbers of the same type | multiplies |
|`div`   | a number              | two numbers of the same type | divides |
|`mod`   | an integer            | two integers                 | modulo |
|`and`   | any type              | two values of any type       | bitwise logical and |
|`or`    | any type              | two values of any type       | bitwise logical or |
|`xor`   | any type              | two values of any type       | bitwise logical exclusive or |
|`not`   | any type              | any type                     | bitwise complement |
|`grows` | an integer (1 or 0)   | two numbers of the same type | checks if the two arguments are increasing, when read from left to right, and returns 1 if they are |
|`shnks` | an integer (1 or 0)   | two numbers of the same type | checks if the two arguments are decreasing, when read from left to right, and returns 1 if they are |
|`equ`   | an integer (1 or 0)   | two numbers of the same type | checks if the two arguments are equal, and returns 1 if they are |
|`if`    | nothing               | any number and a label       | jumps to the label if its first argument is not equal to zero; 'jump if true' |
|`ifn`   | nothing               | any number and a label       | jumps to the label if its first argument is equal to zero; 'jump if not true' |
|`skip_if` | nothing             | any number and a label       | jumps to the end of block that the label refers to, if its first argument is not equal to zero; 'skip if true' |
|`skip_if` | nothing             | any number and a label       | jumps to the end of block that the label refers to, if its first argument is equal to zero; 'skip if not true' |
|`syscall` | any type             | anywhere from 1 to 6 arguments | preforms a linux syscall |

## Building and usage
To build the compiler, first install odin, clone this repository, and then run:
```
$ > odin build .
```
The executable should be `dive`.
To run dive, do:
```
$ > ./dive some_script.dive
```
