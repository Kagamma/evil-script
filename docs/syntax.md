## Table of Contents
- [Introduction](#introduction)
  + [Architecture](#architecture)
- [Overview](#overview)
  + [Imperative and structured](#imperative-and-structured)
  + [Dynamic](#dynamic)
  + [Types](#types)
  + [Scopes](#scopes)
  + [Memory management](#memory-management)
- [Syntax](#syntax)
  + [Code reuse](#code-reuse)
  + [Operators](#operators)
  + [Variable declarations](#variable-declarations)
  + [Statements](#statements)
  + [If block](#if-block)
  + [While block](#while-block)
  + [Do-while block](#do-while-block)
  + [For block](#for-block)
  + [For-in block](#for-in-block)
  + [Switch-case block](#switch-case-block)
  + [Function declaration](#function-declaration)
  + [Function reference](#function-reference)
  + [Anonymous function](#anonymous-function)
  + [Self](#self)
  + [Yield](#yield)
  + [Try-catch](#try-catch)
  + [Comment](#comment)
  + [Import external functions from dynamic libraries](#import-external-functions-from-dynamic-libraries)
  + [Assert](#assert)

## Introduction
Evil script is a simple and lightweight scripting language. It's syntax is influenced by C, Pascal and Lua.

A simple hello world looks like this:
```
writeln('Hello, World!')
```

### Architecture

The compiler itself is a one-pass compiler. It follows Niklaus Wirth’s design, completely skips AST generation and generates binary directly.

Due to the lack of AST, only constant folding and peephole optimizations are implemented.

The virtual machine is stack-based, with super instructions to speed up certain operations.

The virtual machine is capabled of perform multi-threading without the existence of a global lock (GIL), which mean threads can be scheduled concurrently. There's no hand holding and users are responsible for the safety of the code.

Thread shares the same global storage with the main instance while having it's own stack.

Both the global storage and the stack are one-dimensional array. This ensures fast access to both global and local variables.

Each evil script instance has it's own functions and global storage. A script function declared in one instance cannot be called from another instance.

## Overview

### Imperative and structured
Evil script supports structured programming in the style of C. It  supports function and block scoping. Evil script does not require semicolons to separate statements.

Control flow can be achieved using while, for, do / while, if / else, and switch / case statements. Functions are weakly typed and can accept and return any type.

### Dynamic
Evil script is a dynamic language. This means that a variable can point
to any type at runtime.

### Types

```
string, number, boolean, map, buffer, function, pasobject, null
```

Numbers and Booleans are stored as 64-bit floating point number.

Strings are by default in UTF-8 format.

Maps are the only data structure available in evil script that helps us
create different types like arrays and dictionaries. Maps can be indexed
with both numbers and strings. Maps have no fixed size and can grow
based on our need.

For those who familiar with Lua, maps are basically the same as Lua's
tables.

Buffers are used to interface with DLL libraries written in native
languages like C or Pascal. It is basically a space within the system
memory that is used to store raw data for just about anything. Underneath
a buffer is just a pointer and you can perform pointer arimethic on it.

### Scopes
Variables are block-scoped. They are accessible only within the scope in which they are declared and any nested scopes.

Variables declared in a function are local to that function by default. If a global variable has the same name, the `local` prefix is needed. Such variables are stored on the stack.

Evil script does not support closures. Try to access parent function's local variables from a nested function may cause unexpected behaviors.

### Memory management
Strings, Maps, Buffers and managed PasObjects are subject to automatic memory management. You do not have to worry about allocation and deallocation of these data types.

The virtual machine employs a simple mark-and-sweep garbage collector that operates periodically to reclaim memory. This garbage collection algorithm works in two phases: the "mark" phase, where it identifies all objects still in use by tracing references from active program variables, and the "sweep" phase, where it reclaims memory from objects no longer referenced. The collector runs predictably, meaning it is triggered at regular intervals or when specific memory thresholds are reached, balancing performance and memory efficiency.

Users can invoke GC manually via `mem_gc()` function.

## Syntax

### Code reuse
`using 'path/to/source_code.evil'`

### Operators

```
Operator                                        Precedence
==============================================  ==========
 !, sign "-"                                     1
 *, /                                            2
 +, subtract "-"                                 3
 bitshift <<, >>                                 4
 equal "=", !=, <, <=, >, >=, &, |, ~, pow "^"   5
 ternary                                         6
 assign "=", +=, -=, *=, /=                      7

```

```
Bitwise operator     Description
==================== =============
 <<                   Shift left
 >>                   Shift right
 &                    and
 |                    or
 ~                    xor
 !                    not
```

Evil script does not support logic operators, thus it does not distinguish between =, &, | and ==, &&, ||, allowing them to be used interchangeably.

### Variable declarations

```
a = 5
// Assign 5 to a

a += 2 * 5
// a = a + (2 * 5)

a = 'a string'
// 'a' is now a string

b = 'this is ${a}'
// Replace ${a} with the content in variable a. The result will be 'this is a string'
// It is equivalent to: b = 'this is ' + string(a)

b = 'this is
a multi-line
string'

a[2] = 'S'
// Replace char "s" with "S" in string 'a'

c = []
// Create an empty map and store its reference in 'c'
// By default a newly created empty map is considered as a valid array. It will lose it's array status
// once we start adding entries that doesn't make sense to the map.
// Evil script optimizes maps with valid array status, by storing values in an actual array underneath
// for quick access.
// A nice trick to create array with size is to set the last index, for example: c[99] = null
// This will increase array size to 100

c = [ name: 'Satania', race: 'Demon' ]
// Create a map with 2 entries and store its reference in 'c'

c['item'] = 'orange'
// New entry, with key="item" and value="orange"

c.item = 'orange'
// Same as above, but use dot notation instead of square bracket notation

d = [2, 3, 4, 'a string', 'another string']
// Create a map as a valid array, with 5 items

d[1] = 'another string'
// The second element of array 'd' is replaced with 'another string'

d['a'] = 5
// 'd' is no longer a valid array
```

- Strings are copy-on-write, while maps are passed by reference.

### Statements

```
expressions = a + b * c / d + (5 - 2)

s_concat = 'a string ' + "another string"

array_concat = [1, 2, 'pine'] + [5, 7]
// Concat arrays. This only work correctly if both maps are valid array.
// Result in [1, 2, 'pine', 5, 7]

map_concat = ['a': 1, b: 1] + ['b': 2, 'c': 3]
// Concat maps. This only work correctly if both maps are not valid array.
// Result in ['a': 1, 'b': 2, 'c': 3]

and = a & b

or = a | b

not = !a

pow = a^b

bitwise_left_shift = a << b

bitwise_right_shift = a >> b

ternary = a != 5 ? 2 : 4
```

### If block

```
if (a < b) & (c < d) {
  // Do something
} else if (a > b) {
  // Do something
} else {
  // Do something
}
```

### While block

```
i = 0
while i < a {
  if b = i {
    break
  }
  if c = i {
    continue
  }
  i = i + 1
}
```

### Do-while block

```
i = 0
do {
  if b = i {
    break
  }
  if c = i {
    continue
  }
  i = i + 1
} while i < a
```

### For block

```
for i = 0 to 4 {
  if i < 2
    continue
  break
}

for i = 4 downto 0 {
  if i > 2
    continue
  break
}

For loop can accept floating point numbers, and step is not necessary 1:

for i = 0 to 4.2 step 0.2 {
  writeln(i)
}

for i = 4 downto 0 step 0.5 {
  writeln(i)
}

```

### For-in block

For-in block only work correctly with valid array.

```
for value in [1, 2, 5, 7, 9] {
  if value = 5
    break
}
```

```
for value, index in [1, 2, 5, 7, 9] {
  writeln(string(index) + ": " + string(value))
}
```

### Switch-case block

```
a = 5
switch a {
  case 4:
  case 5:
    writeln('4,5')
    break
  case 6:
    writeln('6')
    break
  default:
    writeln('default')
}
```

- Unlike C, Evil script's switch case allows the use of expressions, so the above example can be written like this:

```
a = 5
switch true {
  case (a = 4) | (a = 5):
    writeln('4,5')
    break
  case (a = 6):
    writeln('6')
    break
  default:
    writeln('default')
}
```

- Strings are allowed:

```
s = 'alpha'
switch s {
  case 'alpha':
    writeln('alpha')
    break
  case 'beta':
    writeln('beta')
    break
  case 'gamma':
    writeln('gamma')
}
```

### Function declaration

```
fn foo() {
  fn this_is_a_nested_function() {
    return (true)
  }
  writeln('Hello')
  return (this_is_a_nested_function())
  writeln("This text won't show on screen")
}

fn add(a, b) {
  result = a + b
}

fn sub(a, b) {
  return (a - b)
}

foo()
c = add(5, 3)
```

- Alternative way to declare a function is by returning a function reference:

```
add = fn(a, b) {
  result = a + b
}
```

There're 2 ways to return a value:
- Assign function result to `result` variable
- Use `return`. Note that you need to wrap expression in brackets, for example `return (true)`

Note: While we allow the declaration of nested functions, the lack of closures mean they cannot access any local variables from the parent function if called outside of parent function.

### Function reference

```
fn add(a, b) {
  result = a + b
}
add_ref = add
calc = []
calc.add = add

writeln(add_ref(5, 3)) // Print "8"
writeln(calc.add(2, 4)) // Print "6"
writeln(calc.add = add_ref) // Print "true"
```

### Anonymous function

```
fn test(func) {
  func('Satania')
}

fn calc(f) {
  result = f(5, 3)
}

test(fn(v) writeln('Hello, ${v}!'))
test(fn(v) writeln('Goodbye, ${v}!'))
writeln(calc(fn(a, b) = a + b))
```

- The following function declarations are the same:
```
fn(n) = n + 1

fn(n) result = n + 1

fn(n) { result = n + 1 }
```

### Self

- Evil script does not support true OOP. Instead, "object instance" containing the callee will pass itself to callee as `self`.
- The main reason for this mechanism instead of true OOP is performance: This way it doesn't require a separate "method reference" type, which typically uses twice the memory (which result in additional memory allocations due to the way script engine keeps data), compared to a normal "function reference".
- Nested functions and/or outside functions can access caller's `self`.

```
fn obj_create() {
  fn hello() {
    result = 'Hello, ' + self.name + '!'
  }

  result = [
    name: '',
    hello: hello
  ]
}

obj = obj_create()
obj.name = 'Satania'
writeln(obj.hello()) // obj will be passed to hello() as "self"
```

- Because of the way we pass `self`, one function can be used in multiple object instances, for example:

```
fn test() {
  result = self.value
}

obj1 = [
  value: 1,
  test: test,
]

obj2 = [
  value: 2,
  test: test,
]

writeln(obj1.test())
writeln(obj2.test())
```

### Yield

- If calls outside coroutines: Quit the script and returns to main process. When the process executes the script again, it will continue at where yield's called.

- If calls inside coroutines: Quit the current coroutine. When the script resumes the coroutine, it will continue at where yield's called.

- `yield (expressions)` is equivalent to:
```
  result = expressions
  yield
```

### Try-catch

```
fn test() {
  throw 'Test exception'
}

try {
  writeln('start')
  test()
  writeln('finish')
} catch(e) {
  writeln('Exception: ', e)
}
```

### Comment

```
// A comment

/*
  A
  multi-line
  comment
*/
```

### Import external functions from dynamic libraries
```
import 'test.dll' {
  fn Add(i32, i32): i32
  fn AddDouble(f64, f64): f64
}
import 'user32.dll' fn MessageBox(i32, buffer, buffer, i32): i32 'MessageBoxA' // Map MessageBoxA external function to MessageBox

MessageBox(0, 'Hello, World!', 'Message Box', 0) // Strings are automatically converted to null-terminated strings
```

List of supported data types:
- i8: char
- u8: unsigned char
- i16: short
- u16: unsigned short
- i32: long
- u32: unsigned long
- i64: long long
- u64: unsigned long long
- f32: float
- f64: double
- buffer: char*
- wbuffer: wchar*
- void: This simply tell the app the function does not return any value.

By default, `import` supports `Microsoft x64 calling convention` on Windows, and `System V AMD64 ABI` on Linux. There's no way to change calling convention at the moment.

### Assert
```
assert(expr, 'Error message')
```

With assertions on, `assert` tests if expr is false, and if so, aborts the script with an EAssertionFailed exception. If expr is true, script execution continues normally.
If assertions are not enabled at compile time, this routine does nothing, and no code is generated for the `assert` call.
You can enable assertions globally in Settings, or locally in script editor.