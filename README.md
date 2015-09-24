# README

* Author: lvwenlong_lambda@qq.com
* Last Modified:2015年09月24日 星期四 11时15分55秒 四

## About this project

This is a naive `brainfuck` interpreter, written in Haskell, using the `Parsec` library

## About Brainfuck

* `>` -> `++ptr`
* `<` -> `--ptr`
* `+` -> `++*ptr`
* `-` -> `--*ptr`
* `.` -> `putchar(*ptr)`
* `,` -> `*ptr = getchar()`
* `[` -> `while(*ptr) {`
* `]` -> `}`
