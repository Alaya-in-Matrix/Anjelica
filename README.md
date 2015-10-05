# README

* Author: lvwenlong_lambda@qq.com
* Last Modified:2015年10月05日 星期一 22时23分49秒 一

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

* [Whitespace Wiki](https://en.wikipedia.org/wiki/Whitespace_(programming_language))
* [Whitespace Tutorial](http://compsoc.dur.ac.uk/whitespace/tutorial.html)
