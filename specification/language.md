# Syntax and Semantics of BLisp

## Literal

- $LITERAL := $HEX | $OCT | $BIN | $DECIMAL | $BOOL | $STRING
- $DECIMAL
  - decimal number
  - examples: 0, 100, 224, -130, 4457, 0007
- $HEX
  - hexadecimal number
  - examples: 0xabcd 0xABCD
- $BIN
  - binary number
  - examples: 0b1100, 0b01011
- $BOOL := true | false
- $STRING
  - string literal
  - example: "Hello, World!"
  - escape sequences
    - \r
    - \n
    - \t
    - \0
    - \\\\
    - \"

## Identifier

- $ID
  - a string whose first character is not capital (not 'A' to 'Z')
  - excludes "true" and "false"

## Type Identifier

- $TID
  - a string whose first character is capital ('A' to 'Z')

## Type

- $TYPE := Int | Bool | $TYPE_LIST | $TYPE_TUPLE | $TYPE_FUN | $TYPE_DATA | $ID
- $TYPE_LIST := '( $TYPE )
- $TYPE_TUPLE := \[ $TYPE* \]
- $TYPE_DATA := $TID | ( $TID $TYPE* )
- $TYPE_FUN := ( $EFFECT ( -> $TYPES $TYPE ) )
- $TYPES := ( $TYPE* )
- $EFFECT := Pure | IO

examples:

```common-lisp
'(Int)
[Int Bool]
(Pure (-> (Int INT) Bool))
'('(Int Bool))
[Int Int '([Int Bool])]
```

## Data Type

- $DATA := ( data $DATA_NAME $MEMBER* )
- $DATA_NAME := $TID | ( $TID $ID* )
- $MEMBER := $TID | ( $TID $TYPE* )

examples:

```common-lisp
(data Dim2
  (Dim2 Int Int))

(data (Maybe t)
    (Just t)
    Nothing)

(data (Tree t)
    (Node (Tree t) (Tree t))
    Leaf)
```

## Function Definition

- $DEFUN := ( $HEAD_DEFUN $ID ( $ID* ) $TYPE_FUN $EXPR )
- $HEAD_DEFUN := export | defun

example:

```common-lisp
(defun add (x y) (Pure (-> (Int Int) Int))
  (+ x y))
```

## Expression

- $EXPR := $LITERAL | $ID | $TID | $LET | $IF | $LAMBDA | $MATCH | $LIST | $TUPLE | $GENDATA | $APPLY

### Let Expression

- $LET := ( let ( $DEFVAR+ ) $EXPR )
- $DEFVAR := ( $LETPAT $EXPR )
- $LETPAT := $ID | [ $LETPAT+ ] | ($TID $LETPAT+ )

### If Expression

- $IF := ( if $EXPR $EXPR $EXPR )

### List Expression

- $LIST := '( $EXPR* )

### Tuple Expression

- $TUPLE := [ $EXPR* ]

### Match Expression

- $MATCH := ( match $EXPR $CASE+ )
- $CASE := ( $PATTERN $EXPR )
- $PATTERN := $LITERAL | $ID | $TID | \[ $PATTERN+ \] | ( $TID $PATTERN* ) | '()

### Function Application

- $APPLY := ( $EXPR+ )

### Data Creataion

- $GENDATA := ( $TID $EXPR* )

### Lambda

- $LAMBDA := (lambda ($ID*) $EXPR)

## Built-in Functions

- +, -, *, /, %: (Pure (-> (Int Int) Int))
- band, bor, bxor: (Pure (-> (Int Int) Int))
- pow: (Pure (-> (Int Int) (Some Int)))
- sqrt: (Pure (-> (Int) (Some Int)))
- <, >, <=, >=, =: (Pure (-> (Int Int) Bool))
- and, or, xor: (Pure (-> (Bool Bool) Bool))
- not: (Pure (-> (Bool) Bool))
