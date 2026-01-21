# Novac - The Supernova Compiler

Supernova is a statically typed functional language written in Rust

## Syntax & Semantics

### Functions

```rust
// Basic functions
let add :: a, b = a + b
let sub :: a: i32, b: i32
    -> i32 = a - b

let do_many_things
:: x, y, z = {
    let res := x + y * 3
    let res := y - res ^ 2
    let res := res - z ^ x
    return res
}

// Anonymous functions
let add := |a, b| a + b
let apply :: f: |i32, i32|, a, b
-> i32 = f(a, b)
```

### User Types

```rust
// Enum
#[display]
let Job
:= enum {
    #[display := self.lang <> " dev"]
    programmer: struct { lang: string },
    #[display := "telemarketer who makes $" <> @stringify(self.0)]
    telemarketer: (i32), // salary
    #[display := self]
    other: str,
    // the auto display trait would just be 'unemployed'
    unemployed,
}

// Structs
let Person
:= struct {
    name: str,
    age: u8,
    job: Job,
} with {
    let new
    :: name, age, job
    -> Self = .{
        .name = name,
        .age  = age,
        .job  = job,
    }

    let intro
    :: self = {
        // `@println` and `@display` are compiler builtins
        @println("Hello! My name is" <> self.name <> ".")
        @println("I am {} years old.", self.age)
        @println("And my current job is" <> @display(self.job))
    }
}

let my_person := Person:new("Ashton", 19, .programmer(.{ .lang = "Supernova" }))
my_person.intro()
```

### Control Flow

#### Match

```rust
/* Match on literal */
match my_int {
| 1 -> @println("one"),
| 2, 3 -> @println("zwei oder drei"),
| x if x == 4 -> @println("cuatro"),
| x -> @println("uhhh, I don't know this one..."),
}

/* Match on lists */
match my_list {
| [head, ..] -> do_head(head),
| [_, ..tail] -> do_tail(tail),
| [.., last] -> do_last(last),
| [] -> do_empty(),
| _ -> @unreachable
}

/* Match on strings */

/* `str` is a special type in Supernova
 * In this example, `head` will become
 *  all characters BEFORE delimeter,
 *  and `tail` will be all characters AFTER
 *  the delimeter
 */
let lex :: s, ls -> list<str> =
    match s {
    | word :: ' ' :: tail -> lex(tail, ls.append(word)),
    | word -> ls.append(word)
    | _ -> @unreachable
    }
```

#### If Else Constructs

```rust

let x := 10
let y := 11

if   x < y { @println("{} < {}", x, y) }
elif x > y { @println("{} > {}", x, y) }
else       { @println("{} == {}", x, y) }

```

#### Loops

Simple answer: just use **Recursion**!
