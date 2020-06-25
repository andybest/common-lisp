((paren-group/left
  :token "("
  :precedence 1
  :class parenthetical-grouping
  :associativity nil
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (paren-group/right
  :token ")"
  :precedence 1
  :class parenthetical-grouping
  :associativity nil
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (array-subscript/left
  :token "["
  :precedence 2
  :class array-subscript
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (array-subscript/right
  :token "]"
  :precedence 2
  :class array-subscript
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (function-call/left
  :token "("
  :precedence 2
  :class function-call/constructor
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (function-call/right
  :token ")"
  :precedence 2
  :class function-call/constructor
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (constructor/left
  :token "("
  :precedence 2
  :class function-call/constructor
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (constructor/right
  :token ")"
  :precedence 2
  :class function-call/constructor
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (method-selector
  :token "."
  :precedence 2
  :class structure-field/method-selector/swizzle
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (structure-field
  :token "."
  :precedence 2
  :class structure-field/method-selector/swizzle
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (swizzle
  :token "."
  :precedence 2
  :class structure-field/method-selector/swizzle
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (increment/postfix
  :token "++"
  :precedence 2
  :class increment/postfix
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (decrement/postfix
  :token "--"
  :precedence 2
  :class decrement/postfix
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (increment/prefix
  :token "++"
  :precedence 3
  :class increment/prefix
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (decrement/prefix
  :token "--"
  :precedence 3
  :class decrement/prefix
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (unary/add
  :token "+"
  :precedence 3
  :class unary
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (unary/sub
  :token "-"
  :precedence 3
  :class unary
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (unary/bitwise-not
  :token "~"
  :precedence 3
  :class unary
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (unary/not
  :token "!"
  :precedence 3
  :class unary
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (mult
  :token "*"
  :precedence 4
  :class multiplicative
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (div
  :token "/"
  :precedence 4
  :class multiplicative
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (mod
  :token "%"
  :precedence 4
  :class multiplicative
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (add
  :token "+"
  :precedence 5
  :class additive
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (sub
  :token "-"
  :precedence 5
  :class additive
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (bitwise-shift-left
  :token "<<"
  :precedence 6
  :class bitwise-shift
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (bitwise-shift-right
  :token ">>"
  :precedence 6
  :class bitwise-shift
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (<
  :token "<"
  :precedence 7
  :class relational
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (>
  :token "<"
  :precedence 7
  :class relational
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (<=
  :token "<="
  :precedence 7
  :class relational
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (>=
  :token ">="
  :precedence 7
  :class relational
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (equal
  :token "=="
  :precedence 8
  :class equality
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (not-equal
  :token "!="
  :precedence 8
  :class equality
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (bitwise-and
  :token "&"
  :precedence 9
  :class bitwise-and
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (bitwise-xor
  :token "^"
  :precedence 10
  :class bitwise-xor
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (bitwise-or
  :token "|"
  :precedence 11
  :class bitwise-or
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (and
  :token "&&"
  :precedence 12
  :class logical-and
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (xor
  :token "^^"
  :precedence 13
  :class logical-xor
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (or
  :token "||"
  :precedence 14
  :class logical-or
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (ternary/left
  :token "?"
  :precedence 15
  :class selection
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (ternary/right
  :token ":"
  :precedence 15
  :class selection
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign
  :token "="
  :precedence 16
  :class assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/add
  :token "+="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/sub
  :token "-="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/mult
  :token "*="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/div
  :token "/="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/mod
  :token "%="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/bitwise-shift-left
  :token "<<="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/bitwise-shift-right
  :token ">>="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/bitwise-and
  :token "&="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/bitwise-xor
  :token "^="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (assign/bitwise-or
  :token "|="
  :precedence 16
  :class arithmetic-assignment
  :associativity <-
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460))
 (sequence
  :token ","
  :precedence 16
  :class sequence
  :associativity ->
  :versions (:110 :120 :130 :140 :150 :330 :400 :410 :420 :430 :440 :450 :460)))
