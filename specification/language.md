# Syntax and Semantics of Typed Lisp

## Literal

- $LITERAL := $DECIMAL | $BOOL
- $DECIMAL
  - decimal number
  - examples: 0, 100, 224, -130, 4457
- $BOOL := true | false

## Identifier

- $ID
  - a string whose first character is not captal (not 'A' to 'Z')
  - excludes "true" and "false"

## Type Identifier

- $TID
  - a string whose first character is captal ('A' to 'Z')

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
```
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

- nth: (Pure (-> (Int \[T\]) (Maybe T)))
- nth: (Pure (-> (Int '(T)) (Maybe T)))
- +, -, *, /, %: (Pure (-> (Int Int) Int))
