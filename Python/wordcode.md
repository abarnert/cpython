A few years ago, Cesare di Mauro created a project called [WPython][wpython],
a fork of CPython 2.6.4 that "brings many optimizations and refactorings". The starting point of the project was replacing the bytecode with "wordcode". However, there were a number of other changes on top of it.

I believe it's possible that replacing the bytecode with wordcode would be useful on its own. Of course it could also lead to opportunities for more optimizations and refactorings, but it might be worth keeping a wordcode fork alive (or even proposing it as a patch to core CPython) that doesn't have additional radical changes.

## Bytecode

The core of CPython is an [eval loop][eval] that implements a simple [stack-based virtual machine][stack]. At each step, it has a `frame` object, with an attached `code` object and an instruction pointer. The `code` object contains a `bytes` string full of bytecode. So, it just fetches the next bytecode operation and interprets it. The [compiler][compile] is responsible for turning source code into bytecode, and is called as needed from within the interpreter (by the importer, the `exec` function, etc.).

Each operation consists of a single 8-bit opcode, plus possibly a 16-bit argument. For example, the `RETURN_VALUE` opcode (which returns the value on top of the stack to the caller) doesn't need an argument, so a `RETURN_VALUE` instruction is just a single byte (83). But the `LOAD_CONST` opcode, which loads a constant stored in the `code` object by index, does need an opcode, to tell the VM which index to use, so `LOAD_CONST 0` takes 3 bytes (100, 0, 0).

Argument values that don't fit into 16 bits are handled by prefixing an operation with a special `EXTENDED_ARG` opcode. This obviously rarely comes up with opcodes like `LOAD_CONST`, but may occasionally turn up with, say, `JUMP_ABSOLUTE`. For example, to jump to offset 0x123456 would require `EXTENDED_ARG 18` (the most-significant `0x12`) followed by `JUMP_ABSOLUTE 13398` (the least-significant `0x3456`), which takes 6 bytes (144, 18, 0, 113, 86, 52).

The [`dis`][dis] module documentation explains what each opcode does, and which ones do and don't take arguments.

The biggest potential problem here is that fetching a typical `LOAD_CONST` takes three single-byte fetches: first, because the interpreter doesn't know whether it needs an argument until it sees the opcode, it has to fetch just one byte. Then, when it needs an argument, it has two fetch two more bytes (it could just fetch a word, but half the time that word will be misaligned, which is illegal on some platforms, and no cheaper than--or even more expensive than--two single-byte fetches on others). This also means that the argument fetching has to be either conditional (which can break branch prediction), or duplicated to each opcode's interpreter chunk (which increases cache pressure). Even the more complicated pointer arithmetic can slow things down by preventing the CPU from figuring out what to prefetch.

On top of that, the vast majority of operations with arguments have tiny values (for example, more than half of all `LOAD_CONST` operations in the stdlib have indices 0-3), but they still require two bytes to store those arguments. This makes bytecode longer, meaning more cache spills, disk reads, VM page faults, etc.

Variable-width bytecode is also more complicated to scan, interpret, and modify, which complicates code like the [peephole optimizer][peephole], and third-party libraries like [`byteplay`][byteplay]), as well as the interpreter itself. (Notice the custom macros to fetch the next opcode and argument, peek at the next argument, etc.) Making the code simpler would make it easier to read and maintain, and might open the door for adding further changes. (It might also allow the C compiler or CPU to optimize things better without any work on our part--for example, in some cases, a `PREDICT` macro doesn't always help as much as it could, because the prediction ends up reordered right after a conditional fetch.)

Using two-byte arguments means every argument depends on word-order (CPython stores the arguments in little-endian order, even on big-endian machines).

Finally, variable-width opcodes mean you can't synchronize at an arbitrary position. For example, if I want to disassemble some bytecode around offsets 100-150, there's no way to tell whether offset 100 is an opcode, or part of an argument for an opcode starting at 98 or 99. _Usually_, starting from the middle of an argument will give you a bunch of invalid operations, so you can try 100, then 99, and then 98 until one of them makes sense, but that's not guaranteed to work (and it's not the kind of thing you'd want to automate).

### Packed Arguments

One solution is to find the most commonly-used opcode/argument combinations and pack them into a single byte. So, we'd still have `LOAD_CONST` that takes an argument, but we'd also have `LOAD_CONST_0` and `LOAD_CONST_1` that don't. 

We could extend this to have single-byte-arg variants, so for 2-255 you'd use `LOAD_CONST_B`, and only use `LOAD_CONST_W` for 256 and up.

This would solve many of the problems with existing bytecode--although at the cost of making things more complicated, instead of simpler. It also means duplicating code, or adding jumps, or doing extra mask-and-shift work on the opcodes, any of which could slow things down. (It also involves using up more opcodes, but since we're still only up to 101 out of 255 as of CPython 3.5, this probably isn't too worrying.)

### Wordcode

A different solution is to simplify things to use 16 bits for every operation: one byte for the opcode, one byte for the argument. So, `LOAD_CONST 1` goes from `100, 1, 0` to just `100, 1`. The interpreter can just fetch (aligned) 16-bit words, with no need for conditionals or duplicated code. All else being equal, it should make things faster on most platforms. It also makes things simpler, both in the interpreter and in bytecode processors.

There are two obvious problems here.

First, every opcode that used to take 1 byte now takes 2.

Second, while most arguments are tiny, not all of them are. How can you use an argument >255 with this solution? The obvious answer here is to expand the use of `EXTENDED_ARG`. Keeping things simple, we can allow up to three `EXTENDED_ARG` opcodes, each carrying an additional byte for the following operation (which are then assembled in big-endian order). So, for example, `LOAD_CONST 321` goes from `100, 65, 1` to `144, 1, 100, 65`.

Of course this means that many jumps (where values >255 aren't that rare) now take 4 bytes instead of 3, although that's balanced by many other jumps taking 2 bytes instead of 3. (Also note that if we treat the bytecode as an array of 16-bit words instead of an array of 8-bit bytes, each offset now takes 1 less bit.)

So, putting all of that together, will code get longer or shorter overall? (Remember, shorter code also means a faster interpreter.) It's hard to predict, but I did a quick experiment with the `importlib` frozen code and the `.pyc` files for the stdlib, and it looks like wordcode even with the peephole optimizer disabled is about 7% smaller than bytecode with the optimizer enabled. So, I think we'd get a net savings. But obviously, this is something to test with a more complete implementation, not just guess at. Plus, it's possible that, even if we're saving space overall, we're hitting the worst costs in exactly the worst places (e.g., mid-sized functions may often end with with loop bodies that expand to another cache line), which we'd need to test for.

More extensive use of `EXTENDED_ARG` might also increase register pressure (because the compiler and/or CPU decides to dedicate a register to holding the current extended value rather than use that register for something else).

So, there's no guarantee that this will make things faster, and a possibility that it will make things slower. We won't know until we build something to test.

Besides performance, more extensive use of `EXTENDED_ARG` now means if you write a quick&dirty bytecode processor that just pretends it never exists, it will fail reasonably often instead of very rarely.

On the other hand, Python doesn't have a type for "immutable array of pairs of bytes" akin to `bytes`. Continuing to use the `bytes` type for bytecode could be confusing (especially if jumps use word-based offsets--that means code would need `*2` and `//2` all over the place). But using something like `array('H')` has its own problems (endianness, mutability, not being a builtin). And creating a new builtin `words` type for something that most users are never going to touch seems like overkill.

### Hybrid

Obviously, you can't do both of the above at the same time. The first change is about creating more opcodes that don't need an argument at all, while the second change gives an argument to opcodes even if they don't need it, which would cancel out the benefit.

However, there is a hybrid that might make sense: For opcodes that frequently need values a little bigger than 255, we could steal a few bits from the opcode and use it as part of the argument. For example, we'd have `JUMP_ABSOLUTE_0` through `JUMP_ABSOLUTE_3`, so to jump to offset 564 (0x234), you'd do `JUMP_ABSOLUTE_2 52` instead of `EXTENDED_ARG 2 JUMP_ABSOLUTE 52`.

It's worth noting that all of the problems we're looking at occur in real machine code. For example, the x86 uses variable-width operations, where some opcodes take no arguments, some take one, some take two, and those arguments can even be different widths. And RISC is essentially this hybrid solution--for example, on PowerPC, every operation is 32 bits, with the arguments encoded in anywhere from 0 to 26 of those bits.

## Implementing wordcode

The argument-packing idea is worth exploring on its own. The hybrid idea might be worth exploring as an extension to either wordcode or argument-packing, if they both pan out. But experimenting with just the wordcode idea seems to be worth pursuing.

It's probably worth starting with the smallest possible change. This means bytecode is still treated as a string of bytes, but every operation is now always 2 bytes instead of 1 or 3, and jumps are still byte-offset-based, and no additional simplifications or refactorings are attempted.

So, what needs to be changed?

### Interpreter

The core interpreter loop ([`PyEval_FrameEx`][framex] in `ceval.c`) already has a set of macros to wrap up the complicated fetching of opcodes. We just need to change these macros to always work on 2 bytes instead of conditionally on 1 or 3. Again, this doesn't give us the simplest code; it gives us the code with the fewest changes. This means changing the `FAST_DISPATCH` macro (both versions), `NEXTOP`, `NEXTARG`, `PEEKARG`, `PREDICTED`, and `PREDICTED_WITH_ARG`. The only other place the instruction pointer is manipulated is the `next_instr = first_instr + f->f_lasti + 1;` line under the long comment.

As that comment implies, we also have to patch the frame setup ([`PyFrame_New`][framenew] in `frameobject.c`) to start `f_lasti` at -2 instead of -1, and change the `YIELD_FROM` code inside the eval loop to `-= 2` instead of `--`.

We also need to change `EXTENDED_ARG` to only shift 8 bits instead of 16. (You might expect that we need more changes, to allow up to three `EXTENDED_ARG`s instead of just one, but the code as written already allows multiple instances--although if you use more than one with bytecode, or more than three with wordcode, the extra bits just get shifted out.)

Surprisingly, that's all that needs to be done to interpret wordcode instead of bytecode.

### Compiler

Obviously the compiler (in [`compile.c`][compile]) needs to be changed to emit wordcode instead of bytecode. But again, it's designed pretty flexibly, so it's less work than you'd expect. In particular, it works on a list of instruction objects as long as possible, and only emits the actual bytecode at the very end. This intermediate representation treats instructions with preceding `EXTENDED_ARG` as single instructions. So, it should work unchanged for our purposes.

We need to change the `instrsize` function to return 2, 4, 6, or 8 depending on whether 0, 1, 2, or 3 `EXTENDED_ARG` opcodes will be needed, instead of 1, 3, or 6 for no args, args, or args plus `EXTENDED_ARG`. And we need to modify `assemble_emit` to emit up 0 to 3 `EXTENDED_ARG` opcodes instead of 0 or 1. Finally, the jump target fixup code in `assemble_jump_offsets` has to be similarly modified to count 0 to 3 `EXTENDED_ARG` opcodes instead of 0 or 1.

And that's it for the compiler.

### Peephole optimizer

Unfortunately, the peephole optimizer (in [`peephole.c`][peephole]) doesn't work on that nice intermediate list-of-instructions representation, but on the emitted bytecode. Which means it has to reproduce all the jump-target fixup code, and do it in a more complicated way. It also doesn't process `EXTENDED_ARG` as nicely as the eval loop (it essentially assumes that only `MAKE_FUNCTION` and jump arguments will ever use it, which is no longer almost-always true with wordcode).

For my first proof of concept, I just disabled the peephole optimizer. But obviously, we can't do useful benchmark tests against the stock interpreter this way, so we'll have to tackle it before too long.

As a possible alternative: Victor Stinner's [PEP 511][PEP511] proposes an API for registering AST- and bytecode-based code transformers, and in his earlier work with [FAT Python][FAT] he's reproduced everything the peephole optimizer does (and more) as separate transformers. Most of these should be simpler to port to wordcode (especially since most of them are AST-based, before we even get to the bytecode step). So, it may be simpler to use the PEP 511 patch, disable the peephole optimizer, and use separate optimizers, both for bytecode and for wordcode. We could then test that the 511-ized bytecode interpreter is pretty close to stock CPython, and then fairly compare the 511-ized wordcode intepreter to the 511-ized bytecode interpreter.

### Debugger

The [`pdb`][pdb] debugger has an intimate understanding of Python bytecode, and how it maps back to the compiled source code. Obviously it will need some changes to support wordcode. (This may be another place where we get some simplification opportunities.)

I haven't yet looked at how much work `pdb` needs, but I'm guessing it shouldn't be too hard.

### Introspection tools

The [`dis`][dis] module disassembles bytecode. It obviously needs to be patched. But this turns out to be very easy. There are two functions, `_get_instructions_bytes` and `findlabels`, that each need two changes: to skip or fetch 1 byte instead of fetching 0 or 2 for arguments, and to handle multiple `EXTENDED_ARG`s.

### Marshaling, bootstrapping, etc.

I expected that we might need some changes in marshaling, importer bootstrapping, and other parts of the interpreter. But as it turns out, all of the rest of the code just treats bytecode as an uninterpreted `bytes` object, so all of it just works.

### Third-party code

Of course any third-party disassemblers, decompilers, debuggers, and optimizers that deal with bytecode would have to change.

I believe the most complicated library to fix will be [`byteplay`][byteplay], so my plan is to tackle that one. (It may also be useful for replacing the peephole optimizer, and possibly for debugging any problems that come up along the way.)


  [wpython]: https://code.google.com/archive/p/wpython2/
  [eval]: https://hg.python.org/cpython/file/default/Python/ceval.c#l1121
  [stack]: https://en.wikipedia.org/wiki/Stack_machine
  [compile]: https://hg.python.org/cpython/file/default/Python/compile.c
  [dis]: https://docs.python.org/3/library/dis.html
  [peephole]: https://hg.python.org/cpython/file/default/Python/peephole.c
  [byteplay]: https://wiki.python.org/moin/ByteplayDoc
  [frameex]: https://hg.python.org/cpython/file/default/Python/ceval.c#l796
  [framenew]: https://hg.python.org/cpython/file/default/Objects/frameobject.c#l614
  [PEP511]: https://www.python.org/dev/peps/pep-0511/
  [FAT]: http://faster-cpython.readthedocs.org/fat_python.html
  [pdb]: https://docs.python.org/3/library/pdb.html
  
  
