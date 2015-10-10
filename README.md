# README

* Author: lvwenlong_lambda@qq.com
* Last Modified:CST 2015-10-10 14:57:28 星期六

## About this project

Parsers for some interesting programming language, writtin in Haskell, using `Parsec`

* BrainFuck
* Whitespace
* Elementary Arithmetic

## About Brainfuck

* `>` -> `++ptr`
* `<` -> `--ptr`
* `+` -> `++*ptr`
* `-` -> `--*ptr`
* `.` -> `putchar(*ptr)`
* `,` -> `*ptr = getchar()`
* `[` -> `while(*ptr) {`
* `]` -> `}`

## About Whitespace

* [Whitespace Wiki](https://en.wikipedia.org/wiki/Whitespace_(programming_language))
* [Whitespace Tutorial](http://compsoc.dur.ac.uk/whitespace/tutorial.html)


## About Elementary Arithmetic
* Support `+`, `-`, `*`, `/`
* Support parentheses `(` and `)`
* Interpreter will convert string expression to lisp-style s-expression and evaluate it
