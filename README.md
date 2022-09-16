# nForth - Work In Progress

Summary: a tiny (a few kilobytes) indirect-threaded i386 Forth-like language with a few deviations.  Bootstrap code written in FASM.  This project is entirely self-contained and requires no outside libraries, or anything else except for a way to read and write.

Targeting Linux, and possibly KolibriOS in the future.

## Status

WIP.  Defining and running simple code.

## Deviations from Forth

### Always Compile
To avoid the whole state morass, nForth is *always* in compile mode.  Words are parsed, compiled, immediately executed, and erased.  Defining words adjust the dictionary top to avoid erasure.  No more [IF] and such.

### Hashes not Names
Names are not stored in the dictionary.  Names are hashed at definition time using FNV1a algo to a 32-bit value and stored.  Dictionary searches are simplified to a 32-bit comparison instead of a string comparison, and heads are always 8 bytes long.

## Internals

Tokens are 32-bit references to the XT of each words.  NEXT is a 3-byte piece of code that terminates all CODE words:
```
lodsd
jmp [eax]
```
ESP is used as the return stack, while ebp holds the data stack.  For data operations, ebp and esp are swapped, and esp temporarily holds the data stack.  ESP must be restored prior to control transfers.  Special care must be taken at points of branch targets to assure that the stack is in a known state.

Register usage:
| reg | purpose | user-preserved? |
|-|-|-|
| eax | inner, trashed | no |
| ebx | -- | no |
| ecx | loop count | yes |
| edx | scratch | no |
| esi | IP | yes |
| edi | -- | yes |
| ebp | stack | yes |

### Header format

| offset | size | Description |
|-|-|-|
| 0 | 2 | FLAGS - bit 0 is immediate when set |
| 2 | 2 | LINK - byte offset to previous head |
| 4 | 4 | HASH - hash of the name |
|<-------| | Entry Pointer |||
| 8 | 4 | XT |
...
