# Supernova Grammar

PROGRAM ::= <TOP-LEVEL-EXPR-LIST>

TOP-LEVEL-EXPR-LIST ::=
  | ε
  | <TOP-LEVEL-EXPR-LIST> <TOP-LEVEL-EXPR>
  ;

TOP-LEVEL-EXPR ::=
  | <FUNCTION>
  | <VARIABLE-DECL>
  | <TYPE-DECL>
  ;

## Functions

FUNCTION ::=
  | let <IDENT> <GENERIC-DECL> :: <FUNCTION-PARAM-LIST> = <EXPR-LIST>
  | let <IDENT> <GENERIC-DECL> :: <FUNCTION-PARAM-LIST> -> <TYPE> = <EXPR-LIST>
  ;

NOTE: `let` corresponds to Let keyword token
NOTE: `::` corresponds to DoubleColon token
NOTE: `->` corresponds to RArrow token
NOTE: `=` corresponds to Equal token

FUNCTION-PARAM-LIST ::=
  | <FUNCTION-PARAM>
  | <FUNCTION-PARAM> , <FUNCTION-PARAM-LIST>
  ;

FUNCTION-PARAM ::=
  | <IDENT>
  | <IDENT> : <TYPE>
  ;

## Variable Declarations

VARIABLE-DECL ::=
  | let <IDENT> : = <EXPR>
  | let <IDENT> : <TYPE> = <EXPR>
  ;

NOTE: `:=` is parsed as two tokens: `:` (Colon) followed by `=` (Equal)

## Type Declarations

TYPE-DECL ::=
  | <ATTR-LIST> let <IDENT> := <STRUCT-DECL>
  | <ATTR-LIST> let <IDENT> := <ENUM-DECL>
  ;

ATTR-LIST ::=
  | ε
  | <ATTR> <ATTR-LIST>
  ;

ATTR ::=
  | # [ <IDENT> ]
  | # [ <IDENT> : = <EXPR> ]
  ;

NOTE: `#[` is parsed as `#` (Pound) followed by `[` (LeftSquare)
NOTE: `:=` in attributes is parsed as `:` (Colon) followed by `=` (Equal)

STRUCT-DECL ::=
  | struct { <STRUCT-FIELD-LIST> } <STRUCT-IMPL>
  ;

STRUCT-FIELD-LIST ::=
  | <STRUCT-FIELD>
  | <STRUCT-FIELD> , <STRUCT-FIELD-LIST>
  | ε
  ;

STRUCT-FIELD ::=
  | <IDENT> : <TYPE>
  ;

STRUCT-IMPL ::=
  | ε
  | with { <FUNCTION-LIST> }
  ;

FUNCTION-LIST ::=
  | <FUNCTION>
  | <FUNCTION-LIST> <FUNCTION>
  ;

ENUM-DECL ::=
  | enum { <ENUM-VARIANT-LIST> }
  ;

ENUM-VARIANT-LIST ::=
  | <ENUM-VARIANT>
  | <ENUM-VARIANT> , <ENUM-VARIANT-LIST>
  | ε
  ;

ENUM-VARIANT ::=
  | <ATTR-LIST> <IDENT>
  | <ATTR-LIST> <IDENT> : <TYPE>
  ;

## Types

TYPE ::=
  | <PRIMITIVE-TYPE>
  | <IDENT>
  | <TYPE> <GENERIC-ARGS>
  | <FUNCTION-TYPE>
  | <TUPLE-TYPE>
  ;

PRIMITIVE-TYPE ::=
  | i8 | i16 | i32 | i64 | isize
  | u8 | u16 | u32 | u64 | usize
  | str | char | nil | list
  ;

GENERIC-DECL ::=
  | ε
  | < <GENERIC-PARAM-LIST> >
  ;

GENERIC-PARAM-LIST ::=
  | <IDENT>
  | <IDENT> , <GENERIC-PARAM-LIST>
  ;

GENERIC-ARGS ::=
  | < <TYPE-LIST> >
  ;

NOTE: `<` and `>` in generics correspond to LessThan and GrtrThan tokens
NOTE: The parser must disambiguate between comparison operators and generic brackets based on context
NOTE: In type context, `<` `>` are generics; in expression context, they are comparisons

TYPE-LIST ::=
  | <TYPE>
  | <TYPE> , <TYPE-LIST>
  ;

FUNCTION-TYPE ::=
  | | <TYPE-LIST> |
  ;

NOTE: Function types use Bar tokens: `|Type1, Type2|`
NOTE: Example: `|i32, i32|` represents a function taking two i32 parameters

TUPLE-TYPE ::=
  | ( <TYPE-LIST> )
  ;

## Expressions

EXPR-LIST ::=
  | <EXPR>
  | { <STMT-LIST> }
  ;

STMT-LIST ::=
  | <STMT>
  | <STMT-LIST> <STMT>
  ;

STMT ::=
  | <VARIABLE-DECL>
  | <EXPR>
  | return <EXPR>
  | return
  ;

EXPR ::=
  | <EXPR-BINARY>
  ;

EXPR-BINARY ::=
  | <EXPR-UNARY>
  | <EXPR-BINARY> <BIN-OP> <EXPR-UNARY>
  ;

BIN-OP ::=
  | + | - | * | / | % | ^
  | == | != | < | > | <= | >=
  | <> | and | or
  ;

NOTE: Operators correspond to lexer tokens:
  - + (Plus), - (Minus), * (Star), / (Slash), % (Perc), ^ (Carrot)
  - == (DoubleEqual), != (BangEqual), < (LessThan), > (GrtrThan)
  - <= (LessEqual), >= (GrtrEqual)
  - <> (Concat), and (And keyword), or (Or keyword)

EXPR-UNARY ::=
  | <EXPR-PRIMARY>
  | - <EXPR-UNARY>
  | ! <EXPR-UNARY>
  ;

NOTE: Unary operators correspond to lexer tokens:
  - - (Minus), ! (Bang)

EXPR-PRIMARY ::=
  | <LITERAL>
  | <IDENT>
  | <EXPR-CALL>
  | <EXPR-MEMBER>
  | <EXPR-STRUCT-LIT>
  | <EXPR-ENUM-VARIANT>
  | <EXPR-ANON-FN>
  | <EXPR-MATCH>
  | <EXPR-IF>
  | <EXPR-LIST-LIT>
  | <EXPR-TUPLE>
  | ( <EXPR> )
  ;

EXPR-CALL ::=
  | <EXPR-PRIMARY> ( <ARG-LIST> )
  | <EXPR-PRIMARY> : <IDENT> ( <ARG-LIST> )
  ;

NOTE: `:` corresponds to Colon token
NOTE: Method calls use `:` syntax: `Type:method(args)`

ARG-LIST ::=
  | ε
  | <EXPR>
  | <EXPR> , <ARG-LIST>
  ;

EXPR-MEMBER ::=
  | <EXPR-PRIMARY> . <IDENT>
  | <EXPR-PRIMARY> . <INTEGER>
  ;

EXPR-STRUCT-LIT ::=
  | .{ <STRUCT-FIELD-INIT-LIST> }
  | <TYPE> { <STRUCT-FIELD-INIT-LIST> }
  ;

STRUCT-FIELD-INIT-LIST ::=
  | ε
  | <STRUCT-FIELD-INIT>
  | <STRUCT-FIELD-INIT> , <STRUCT-FIELD-INIT-LIST>
  ;

STRUCT-FIELD-INIT ::=
  | . <IDENT> = <EXPR>
  | <IDENT> = <EXPR>
  ;

EXPR-ENUM-VARIANT ::=
  | . <IDENT>
  | . <IDENT> ( <EXPR> )
  | . <IDENT> ( <ARG-LIST> )
  ;

EXPR-ANON-FN ::=
  | | <PARAM-LIST> | <EXPR>
  | | <PARAM-LIST> | { <STMT-LIST> }
  ;

NOTE: `|` corresponds to Bar token
NOTE: Anonymous function syntax uses Bar tokens: `|params| body`

PARAM-LIST ::=
  | ε
  | <IDENT>
  | <IDENT> , <PARAM-LIST>
  ;

EXPR-MATCH ::=
  | match <EXPR> { <MATCH-ARM-LIST> }
  ;

NOTE: `match` is parsed as an identifier (Ident token), not a keyword
NOTE: The lexer does not currently have a Match keyword token

MATCH-ARM-LIST ::=
  | <MATCH-ARM>
  | <MATCH-ARM-LIST> <MATCH-ARM>
  ;

MATCH-ARM ::=
  | | <PATTERN-LIST> -> <EXPR> ,
  | | <PATTERN-LIST> -> <EXPR>
  | | <PATTERN> if <EXPR> -> <EXPR> ,
  | | <PATTERN> if <EXPR> -> <EXPR>
  ;

NOTE: `|` corresponds to Bar token
NOTE: `->` corresponds to RArrow token
NOTE: `if` corresponds to If keyword token

PATTERN-LIST ::=
  | <PATTERN>
  | <PATTERN> , <PATTERN-LIST>
  ;

PATTERN ::=
  | <PATTERN-LITERAL>
  | <PATTERN-IDENT>
  | <PATTERN-WILDCARD>
  | <PATTERN-LIST-PAT>
  | <PATTERN-STR-PAT>
  | <PATTERN-STRUCT>
  | <PATTERN-ENUM>
  | <PATTERN-TUPLE>
  ;

PATTERN-LITERAL ::=
  | <NUMBER>
  | <STRING>
  | <CHAR>
  | <BOOLEAN>
  | <NIL>
  ;

PATTERN-IDENT ::=
  | <IDENT>
  ;

PATTERN-WILDCARD ::=
  | _
  ;

PATTERN-LIST-PAT ::=
  | []
  | [ <PATTERN> ]
  | [ <PATTERN> , .. ]
  | [ .. , <PATTERN> ]
  | [ <PATTERN> , .. , <PATTERN> ]
  | [ <PATTERN-LIST> ]
  ;

NOTE: `..` in list patterns is parsed as two `.` (Dot) tokens
NOTE: The parser must recognize `..` as a special pattern syntax

PATTERN-STR-PAT ::=
  | <IDENT> :: <CHAR> :: <IDENT>
  | <IDENT>
  ;

NOTE: `::` corresponds to DoubleColon token
NOTE: String pattern matching uses DoubleColon to separate string segments

PATTERN-STRUCT ::=
  | .{ <STRUCT-FIELD-PAT-LIST> }
  | <IDENT> { <STRUCT-FIELD-PAT-LIST> }
  ;

STRUCT-FIELD-PAT-LIST ::=
  | ε
  | <STRUCT-FIELD-PAT>
  | <STRUCT-FIELD-PAT> , <STRUCT-FIELD-PAT-LIST>
  ;

STRUCT-FIELD-PAT ::=
  | . <IDENT> = <PATTERN>
  | <IDENT> = <PATTERN>
  | <IDENT>
  ;

PATTERN-ENUM ::=
  | . <IDENT>
  | . <IDENT> ( <PATTERN> )
  | . <IDENT> ( <PATTERN-LIST> )
  ;

PATTERN-TUPLE ::=
  | ( <PATTERN-LIST> )
  ;

EXPR-IF ::=
  | if <EXPR> { <STMT-LIST> }
  | if <EXPR> { <STMT-LIST> } elif <EXPR> { <STMT-LIST> }
  | if <EXPR> { <STMT-LIST> } else { <STMT-LIST> }
  | if <EXPR> { <STMT-LIST> } <ELIF-LIST> else { <STMT-LIST> }
  ;

ELIF-LIST ::=
  | elif <EXPR> { <STMT-LIST> }
  | <ELIF-LIST> elif <EXPR> { <STMT-LIST> }
  ;

EXPR-LIST-LIT ::=
  | []
  | [ <EXPR-LIST> ]
  ;

EXPR-TUPLE ::=
  | ( <EXPR> , <EXPR-LIST> )
  ;

## Literals

LITERAL ::=
  | <NUMBER>
  | <STRING>
  | <BOOLEAN>
  | <NIL>
  | <BUILTIN-CALL>
  ;

NUMBER ::=
  | <NUMBER-LIT>
  ;

NUMBER-LIT ::=
  | (token: NumberLit)
  ;

NOTE: NumberLit is a u64 value tokenized by the lexer
NOTE: The lexer currently only supports decimal integers (hex/oct/bin not yet implemented)

BOOLEAN ::=
  | true
  | false
  ;

NIL ::=
  | nil
  ;

STRING ::=
  | (token: StringLit)
  ;

NOTE: StringLit is tokenized by the lexer as a string literal between `"` and `"`
NOTE: The lexer handles string termination errors

CHAR ::=
  | ' <CHAR-CONTENT> '
  ;

CHAR-CONTENT ::=
  | (any single character except ' and \)
  | <ESCAPE-SEQ>
  ;

NOTE: Character literals may be handled as single-character strings or special-cased
NOTE: The lexer does not have a dedicated CharLit token, but `' '` appears in examples

BUILTIN-CALL ::=
  | @ <IDENT> ( <ARG-LIST> )
  | @ <IDENT>
  ;

## Identifiers

IDENT ::=
  | <IDENT-START> <IDENT-CONTINUE>*
  ;

IDENT-START ::=
  | a-z | A-Z | _
  ;

IDENT-CONTINUE ::=
  | <IDENT-START> | 0-9
  ;

## Lexer Token Reference

### Keywords
- `let`, `if`, `elif`, `else`, `return`
- `struct`, `enum`, `with`
- `and`, `or`
- `true`, `false`, `nil`

NOTE: `match` is not a keyword token but is parsed as an identifier (Ident)

### Operators
- Arithmetic: `+` (Plus), `-` (Minus), `*` (Star), `/` (Slash), `%` (Perc), `^` (Carrot)
- Comparison: `==` (DoubleEqual), `!=` (BangEqual), `<` (LessThan), `>` (GrtrThan), `<=` (LessEqual), `>=` (GrtrEqual)
- String: `<>` (Concat)
- Logical: `and` (And), `or` (Or)
- Unary: `-` (Minus), `!` (Bang)
- Other: `->` (RArrow), `::` (DoubleColon), `|>` (Pipe - not currently used)

### Punctuation
- `@` (At), `#` (Pound), `.` (Dot), `:` (Colon), `,` (Comma), `;` (SemiColon)
- `|` (Bar), `(` (LeftParen), `)` (RightParen)
- `{` (LeftBrace), `}` (RightBrace)
- `[` (LeftSquare), `]` (RightSquare)
- `=` (Equal)

### Literals
- `Ident` - identifier token
- `NumberLit(u64)` - numeric literal
- `StringLit` - string literal

## Notes

- Operator precedence (from highest to lowest):
  1. Unary operators (-, !)
  2. Exponentiation (^)
  3. Multiplicative (*, /, %)
  4. Additive (+, -, <>)
  5. Comparison (==, !=, <, >, <=, >=)
  6. Logical AND (and)
  7. Logical OR (or)

- String concatenation uses `<>` (Concat) operator

- Method calls use `:` (Colon) syntax: `Type:method(args)`
- Member access uses `.` (Dot) syntax: `obj.field`

- List patterns:
  - `[head, ..]` - head and rest
  - `[.., tail]` - all but last, and last
  - `[head, .., tail]` - head, middle, tail
  - `[]` - empty list

- String patterns use `::` (DoubleColon) for pattern matching on string structure

- Comments: `//` for single-line comments, `/* */` for multi-line (multiline not yet implemented in lexer)

- The lexer tokenizes `:=` as two separate tokens: `:` (Colon) followed by `=` (Equal)
- The lexer tokenizes `#[` as two separate tokens: `#` (Pound) followed by `[` (LeftSquare)
