# README

* Author: lvwenlong_lambda@qq.com
* Last Modified:2015年10月03日 星期六 19时03分47秒 六

## About this project

Parsers for some interesting programming language, writtin in Haskell, using `Parsec`

* brainfuck
* Whitespace

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

[Whitespace Wiki](https://en.wikipedia.org/wiki/Whitespace_(programming_language))
[Whitespace Tutorial](http://compsoc.dur.ac.uk/whitespace/tutorial.html)
