# nForth - Work In Progress

Summary: a tiny (a few kilobytes) indirect-threaded i386 Forth-like language with a few deviations.  Bootstrap code written in FASM.  This project is entirely self-contained and requires no outside libraries, or anything else except for a way to read and write.

Targeting Linux, and possibly KolibriOS in the future.

## Status

WIP.  Defining and running simple code.

## Deviations from Forth

To avoid the whole state morass, nForth is *always* in compile mode.  Words are parsed, compiled, immediately executed, and erased.  Defining words adjust the dictionary top to avoid erasure.  No more [IF] and such.

