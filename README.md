# README

* Author: lvwenlong_lambda@qq.com
* Last Modified:2015年09月29日 星期二 23时36分52秒 二

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
