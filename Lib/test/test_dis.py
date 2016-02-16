# Minimal tests for dis module

from test.support import captured_stdout
from test.bytecode_helper import BytecodeTestCase
import difflib
import unittest
import sys
import dis
import io
import re
import types
import contextlib

def get_tb():
    def _error():
        try:
            1 / 0
        except Exception as e:
            tb = e.__traceback__
        return tb

    tb = _error()
    while tb.tb_next:
        tb = tb.tb_next
    return tb

TRACEBACK_CODE = get_tb().tb_frame.f_code

class _C:
    def __init__(self, x):
        self.x = x == 1

dis_c_instance_method = """\
 %-4d         0 LOAD_FAST                1 (x)
              2 LOAD_CONST               1 (1)
              4 COMPARE_OP               2 (==)
              6 LOAD_FAST                0 (self)
              8 STORE_ATTR               0 (x)
             10 LOAD_CONST               0 (None)
             12 RETURN_VALUE             0
""" % (_C.__init__.__code__.co_firstlineno + 1,)

dis_c_instance_method_bytes = """\
          0 LOAD_FAST                1 (1)
          2 LOAD_CONST               1 (1)
          4 COMPARE_OP               2 (==)
          6 LOAD_FAST                0 (0)
          8 STORE_ATTR               0 (0)
         10 LOAD_CONST               0 (0)
         12 RETURN_VALUE             0
"""

def _f(a):
    print(a)
    return 1

dis_f = """\
 %-4d         0 LOAD_GLOBAL              0 (print)
              2 LOAD_FAST                0 (a)
              4 CALL_FUNCTION            1 (1 positional, 0 keyword pair)
              6 POP_TOP                  0

 %-4d         8 LOAD_CONST               1 (1)
             10 RETURN_VALUE             0
""" % (_f.__code__.co_firstlineno + 1,
       _f.__code__.co_firstlineno + 2)


dis_f_co_code = """\
          0 LOAD_GLOBAL              0 (0)
          2 LOAD_FAST                0 (0)
          4 CALL_FUNCTION            1 (1 positional, 0 keyword pair)
          6 POP_TOP                  0
          8 LOAD_CONST               1 (1)
         10 RETURN_VALUE             0
"""


def bug708901():
    for res in range(1,
                     10):
        pass

dis_bug708901 = """\
 %-4d         0 SETUP_LOOP              23 (to 26)
              2 LOAD_GLOBAL              0 (range)
              4 LOAD_CONST               1 (1)

 %-4d         6 LOAD_CONST               2 (10)
              8 CALL_FUNCTION            2 (2 positional, 0 keyword pair)
             10 GET_ITER                 0
        >>   12 FOR_ITER                 6 (to 18)
             14 STORE_FAST               0 (res)

 %-4d        16 JUMP_ABSOLUTE           12
        >>   18 POP_BLOCK                0
        >>   20 LOAD_CONST               0 (None)
             22 RETURN_VALUE             0
""" % (bug708901.__code__.co_firstlineno + 1,
       bug708901.__code__.co_firstlineno + 2,
       bug708901.__code__.co_firstlineno + 3)


def bug1333982(x=[]):
    assert 0, ([s for s in x] +
              1)
    pass

dis_bug1333982 = """\
%3d           0 LOAD_CONST               1 (0)
              2 POP_JUMP_IF_TRUE        26
              4 LOAD_GLOBAL              0 (AssertionError)
              6 LOAD_CONST               2 (<code object <listcomp> at 0x..., file "%s", line %d>)
              8 LOAD_CONST               3 ('bug1333982.<locals>.<listcomp>')
             10 MAKE_FUNCTION            0
             12 LOAD_FAST                0 (x)
             14 GET_ITER                 0
             16 CALL_FUNCTION            1 (1 positional, 0 keyword pair)

%3d          18 LOAD_CONST               4 (1)
             20 BINARY_ADD               0
             22 CALL_FUNCTION            1 (1 positional, 0 keyword pair)
             24 RAISE_VARARGS            1

%3d     >>   26 LOAD_CONST               0 (None)
             28 RETURN_VALUE             0
""" % (bug1333982.__code__.co_firstlineno + 1,
       __file__,
       bug1333982.__code__.co_firstlineno + 1,
       bug1333982.__code__.co_firstlineno + 2,
       bug1333982.__code__.co_firstlineno + 3)

_BIG_LINENO_FORMAT = """\
%3d           0 LOAD_GLOBAL              0 (spam)
              2 POP_TOP                  0
              4 LOAD_CONST               0 (None)
              6 RETURN_VALUE             0
"""

dis_module_expected_results = """\
Disassembly of f:
  4           0 LOAD_CONST               0 (None)
              2 RETURN_VALUE             0

Disassembly of g:
  5           0 LOAD_CONST               0 (None)
              2 RETURN_VALUE             0

"""

expr_str = "x + 1"

dis_expr_str = """\
  1           0 LOAD_NAME                0 (x)
              2 LOAD_CONST               0 (1)
              4 BINARY_ADD               0
              6 RETURN_VALUE             0
"""

simple_stmt_str = "x = x + 1"

dis_simple_stmt_str = """\
  1           0 LOAD_NAME                0 (x)
              2 LOAD_CONST               0 (1)
              4 BINARY_ADD               0
              6 STORE_NAME               0 (x)
              8 LOAD_CONST               1 (None)
             10 RETURN_VALUE             0
"""

compound_stmt_str = """\
x = 0
while 1:
    x += 1"""
# Trailing newline has been deliberately omitted

dis_compound_stmt_str = """\
  1           0 LOAD_CONST               0 (0)
              2 STORE_NAME               0 (x)

  2           4 SETUP_LOOP              14 (to 18)

  3     >>    6 LOAD_NAME                0 (x)
              8 LOAD_CONST               1 (1)
             10 INPLACE_ADD              0
             12 STORE_NAME               0 (x)
             14 JUMP_ABSOLUTE            6
             16 POP_BLOCK                0
        >>   18 LOAD_CONST               2 (None)
             20 RETURN_VALUE             0
"""

dis_traceback = """\
 %-4d         0 SETUP_EXCEPT            12 (to 14)

 %-4d         2 LOAD_CONST               1 (1)
              4 LOAD_CONST               2 (0)
    -->       6 BINARY_TRUE_DIVIDE       0
              8 POP_TOP                  0
             10 POP_BLOCK                0
             12 JUMP_FORWARD            46 (to 54)

 %-4d   >>   14 DUP_TOP
             16 LOAD_GLOBAL              0 (Exception)
             18 COMPARE_OP              10 (exception match)
             20 POP_JUMP_IF_FALSE       60
             22 POP_TOP                  0
             24 STORE_FAST               0 (e)
             26 POP_TOP                  0
             28 SETUP_FINALLY           14 (to 42)

 %-4d        30 LOAD_FAST                0 (e)
             32 LOAD_ATTR                1 (__traceback__)
             34 STORE_FAST               1 (tb)
             36 POP_BLOCK                0
             38 POP_EXCEPT               0
             40 LOAD_CONST               0 (None)
        >>   42 LOAD_CONST               0 (None)
             44 STORE_FAST               0 (e)
             46 DELETE_FAST              0 (e)
             48 END_FINALLY              0
             50 JUMP_FORWARD             1 (to 54)
        >>   52 END_FINALLY              0

 %-4d   >>   54 LOAD_FAST                1 (tb)
             56 RETURN_VALUE             0
""" % (TRACEBACK_CODE.co_firstlineno + 1,
       TRACEBACK_CODE.co_firstlineno + 2,
       TRACEBACK_CODE.co_firstlineno + 3,
       TRACEBACK_CODE.co_firstlineno + 4,
       TRACEBACK_CODE.co_firstlineno + 5)

def _g(x):
    yield x

class DisTests(unittest.TestCase):

    def get_disassembly(self, func, lasti=-1, wrapper=True):
        # We want to test the default printing behaviour, not the file arg
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            if wrapper:
                dis.dis(func)
            else:
                dis.disassemble(func, lasti)
        return output.getvalue()

    def get_disassemble_as_string(self, func, lasti=-1):
        return self.get_disassembly(func, lasti, False)

    def strip_addresses(self, text):
        return re.sub(r'\b0x[0-9A-Fa-f]+\b', '0x...', text)

    def do_disassembly_test(self, func, expected):
        got = self.get_disassembly(func)
        if got != expected:
            got = self.strip_addresses(got)
        self.assertEqual(got, expected)

    def test_opmap(self):
        self.assertEqual(dis.opmap["NOP"], 9)
        self.assertIn(dis.opmap["LOAD_CONST"], dis.hasconst)
        self.assertIn(dis.opmap["STORE_NAME"], dis.hasname)

    def test_opname(self):
        self.assertEqual(dis.opname[dis.opmap["LOAD_FAST"]], "LOAD_FAST")

    def test_boundaries(self):
        self.assertEqual(dis.opmap["EXTENDED_ARG"], dis.EXTENDED_ARG)

    def test_dis(self):
        self.do_disassembly_test(_f, dis_f)

    def test_bug_708901(self):
        self.do_disassembly_test(bug708901, dis_bug708901)

    def test_bug_1333982(self):
        # This one is checking bytecodes generated for an `assert` statement,
        # so fails if the tests are run with -O.  Skip this test then.
        if not __debug__:
            self.skipTest('need asserts, run without -O')

        self.do_disassembly_test(bug1333982, dis_bug1333982)

    def test_big_linenos(self):
        def func(count):
            namespace = {}
            func = "def foo():\n " + "".join(["\n "] * count + ["spam\n"])
            exec(func, namespace)
            return namespace['foo']

        # Test all small ranges
        for i in range(1, 300):
            expected = _BIG_LINENO_FORMAT % (i + 2)
            self.do_disassembly_test(func(i), expected)

        # Test some larger ranges too
        for i in range(300, 5000, 10):
            expected = _BIG_LINENO_FORMAT % (i + 2)
            self.do_disassembly_test(func(i), expected)

        from test import dis_module
        self.do_disassembly_test(dis_module, dis_module_expected_results)

    def test_disassemble_str(self):
        self.do_disassembly_test(expr_str, dis_expr_str)
        self.do_disassembly_test(simple_stmt_str, dis_simple_stmt_str)
        self.do_disassembly_test(compound_stmt_str, dis_compound_stmt_str)

    def test_disassemble_bytes(self):
        self.do_disassembly_test(_f.__code__.co_code, dis_f_co_code)

    def test_disassemble_method(self):
        self.do_disassembly_test(_C(1).__init__, dis_c_instance_method)

    def test_disassemble_method_bytes(self):
        method_bytecode = _C(1).__init__.__code__.co_code
        self.do_disassembly_test(method_bytecode, dis_c_instance_method_bytes)

    def test_disassemble_generator(self):
        gen_func_disas = self.get_disassembly(_g)  # Disassemble generator function
        gen_disas = self.get_disassembly(_g(1))  # Disassemble generator itself
        self.assertEqual(gen_disas, gen_func_disas)

    def test_dis_none(self):
        try:
            del sys.last_traceback
        except AttributeError:
            pass
        self.assertRaises(RuntimeError, dis.dis, None)

    def test_dis_traceback(self):
        try:
            del sys.last_traceback
        except AttributeError:
            pass

        try:
            1/0
        except Exception as e:
            tb = e.__traceback__
            sys.last_traceback = tb

        tb_dis = self.get_disassemble_as_string(tb.tb_frame.f_code, tb.tb_lasti)
        self.do_disassembly_test(None, tb_dis)

    def test_dis_object(self):
        self.assertRaises(TypeError, dis.dis, object())

class DisWithFileTests(DisTests):

    # Run the tests again, using the file arg instead of print
    def get_disassembly(self, func, lasti=-1, wrapper=True):
        output = io.StringIO()
        if wrapper:
            dis.dis(func, file=output)
        else:
            dis.disassemble(func, lasti, file=output)
        return output.getvalue()



code_info_code_info = """\
Name:              code_info
Filename:          (.*)
Argument count:    1
Kw-only arguments: 0
Number of locals:  1
Stack size:        3
Flags:             OPTIMIZED, NEWLOCALS, NOFREE
Constants:
   0: %r
Names:
   0: _format_code_info
   1: _get_code_object
Variable names:
   0: x""" % (('Formatted details of methods, functions, or code.',)
              if sys.flags.optimize < 2 else (None,))

@staticmethod
def tricky(x, y, z=True, *args, c, d, e=[], **kwds):
    def f(c=c):
        print(x, y, z, c, d, e, f)
    yield x, y, z, c, d, e, f

code_info_tricky = """\
Name:              tricky
Filename:          (.*)
Argument count:    3
Kw-only arguments: 3
Number of locals:  8
Stack size:        7
Flags:             OPTIMIZED, NEWLOCALS, VARARGS, VARKEYWORDS, GENERATOR
Constants:
   0: None
   1: <code object f at (.*), file "(.*)", line (.*)>
   2: 'tricky.<locals>.f'
Variable names:
   0: x
   1: y
   2: z
   3: c
   4: d
   5: e
   6: args
   7: kwds
Cell variables:
   0: [edfxyz]
   1: [edfxyz]
   2: [edfxyz]
   3: [edfxyz]
   4: [edfxyz]
   5: [edfxyz]"""
# NOTE: the order of the cell variables above depends on dictionary order!

co_tricky_nested_f = tricky.__func__.__code__.co_consts[1]

code_info_tricky_nested_f = """\
Name:              f
Filename:          (.*)
Argument count:    1
Kw-only arguments: 0
Number of locals:  1
Stack size:        8
Flags:             OPTIMIZED, NEWLOCALS, NESTED
Constants:
   0: None
Names:
   0: print
Variable names:
   0: c
Free variables:
   0: [edfxyz]
   1: [edfxyz]
   2: [edfxyz]
   3: [edfxyz]
   4: [edfxyz]
   5: [edfxyz]"""

code_info_expr_str = """\
Name:              <module>
Filename:          <disassembly>
Argument count:    0
Kw-only arguments: 0
Number of locals:  0
Stack size:        2
Flags:             NOFREE
Constants:
   0: 1
Names:
   0: x"""

code_info_simple_stmt_str = """\
Name:              <module>
Filename:          <disassembly>
Argument count:    0
Kw-only arguments: 0
Number of locals:  0
Stack size:        2
Flags:             NOFREE
Constants:
   0: 1
   1: None
Names:
   0: x"""

code_info_compound_stmt_str = """\
Name:              <module>
Filename:          <disassembly>
Argument count:    0
Kw-only arguments: 0
Number of locals:  0
Stack size:        2
Flags:             NOFREE
Constants:
   0: 0
   1: 1
   2: None
Names:
   0: x"""


async def async_def():
    await 1
    async for a in b: pass
    async with c as d: pass

code_info_async_def = """\
Name:              async_def
Filename:          (.*)
Argument count:    0
Kw-only arguments: 0
Number of locals:  2
Stack size:        17
Flags:             OPTIMIZED, NEWLOCALS, GENERATOR, NOFREE, COROUTINE
Constants:
   0: None
   1: 1"""

class CodeInfoTests(unittest.TestCase):
    test_pairs = [
      (dis.code_info, code_info_code_info),
      (tricky, code_info_tricky),
      (co_tricky_nested_f, code_info_tricky_nested_f),
      (expr_str, code_info_expr_str),
      (simple_stmt_str, code_info_simple_stmt_str),
      (compound_stmt_str, code_info_compound_stmt_str),
      (async_def, code_info_async_def)
    ]

    def test_code_info(self):
        self.maxDiff = 1000
        for x, expected in self.test_pairs:
            self.assertRegex(dis.code_info(x), expected)

    def test_show_code(self):
        self.maxDiff = 1000
        for x, expected in self.test_pairs:
            with captured_stdout() as output:
                dis.show_code(x)
            self.assertRegex(output.getvalue(), expected+"\n")
            output = io.StringIO()
            dis.show_code(x, file=output)
            self.assertRegex(output.getvalue(), expected)

    def test_code_info_object(self):
        self.assertRaises(TypeError, dis.code_info, object())

    def test_pretty_flags_no_flags(self):
        self.assertEqual(dis.pretty_flags(0), '0x0')


# Fodder for instruction introspection tests
#   Editing any of these may require recalculating the expected output
def outer(a=1, b=2):
    def f(c=3, d=4):
        def inner(e=5, f=6):
            print(a, b, c, d, e, f)
        print(a, b, c, d)
        return inner
    print(a, b, '', 1, [], {}, "Hello world!")
    return f

def jumpy():
    # This won't actually run (but that's OK, we only disassemble it)
    for i in range(10):
        print(i)
        if i < 4:
            continue
        if i > 6:
            break
    else:
        print("I can haz else clause?")
    while i:
        print(i)
        i -= 1
        if i > 6:
            continue
        if i < 4:
            break
    else:
        print("Who let lolcatz into this test suite?")
    try:
        1 / 0
    except ZeroDivisionError:
        print("Here we go, here we go, here we go...")
    else:
        with i as dodgy:
            print("Never reach this")
    finally:
        print("OK, now we're done")

# End fodder for opinfo generation tests
expected_outer_line = 1
_line_offset = outer.__code__.co_firstlineno - 1
code_object_f = outer.__code__.co_consts[3]
expected_f_line = code_object_f.co_firstlineno - _line_offset
code_object_inner = code_object_f.co_consts[3]
expected_inner_line = code_object_inner.co_firstlineno - _line_offset
expected_jumpy_line = 1

# The following lines are useful to regenerate the expected results after
# either the fodder is modified or the bytecode generation changes
# After regeneration, update the references to code_object_f and
# code_object_inner before rerunning the tests

#_instructions = dis.get_instructions(outer, first_line=expected_outer_line)
#print('expected_opinfo_outer = [\n  ',
      #',\n  '.join(map(str, _instructions)), ',\n]', sep='')
#_instructions = dis.get_instructions(outer(), first_line=expected_f_line)
#print('expected_opinfo_f = [\n  ',
      #',\n  '.join(map(str, _instructions)), ',\n]', sep='')
#_instructions = dis.get_instructions(outer()(), first_line=expected_inner_line)
#print('expected_opinfo_inner = [\n  ',
      #',\n  '.join(map(str, _instructions)), ',\n]', sep='')
#_instructions = dis.get_instructions(jumpy, first_line=expected_jumpy_line)
#print('expected_opinfo_jumpy = [\n  ',
      #',\n  '.join(map(str, _instructions)), ',\n]', sep='')


Instruction = dis.Instruction
expected_opinfo_outer = [
  Instruction(opname='LOAD_CONST', opcode=100, arg=1, argval=3, argrepr='3', offset=0, starts_line=2, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=2, argval=4, argrepr='4', offset=2, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CLOSURE', opcode=135, arg=0, argval='a', argrepr='a', offset=4, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CLOSURE', opcode=135, arg=1, argval='b', argrepr='b', offset=6, starts_line=None, is_jump_target=False),
  Instruction(opname='BUILD_TUPLE', opcode=102, arg=2, argval=2, argrepr='', offset=8, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=3, argval=code_object_f, argrepr=repr(code_object_f), offset=10, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=4, argval='outer.<locals>.f', argrepr="'outer.<locals>.f'", offset=12, starts_line=None, is_jump_target=False),
  Instruction(opname='MAKE_CLOSURE', opcode=134, arg=2, argval=2, argrepr='', offset=14, starts_line=None, is_jump_target=False),
  Instruction(opname='STORE_FAST', opcode=125, arg=2, argval='f', argrepr='f', offset=16, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=0, argval='print', argrepr='print', offset=18, starts_line=7, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=0, argval='a', argrepr='a', offset=20, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=1, argval='b', argrepr='b', offset=22, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=5, argval='', argrepr="''", offset=24, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=6, argval=1, argrepr='1', offset=26, starts_line=None, is_jump_target=False),
  Instruction(opname='BUILD_LIST', opcode=103, arg=0, argval=0, argrepr='', offset=28, starts_line=None, is_jump_target=False),
  Instruction(opname='BUILD_MAP', opcode=105, arg=0, argval=0, argrepr='', offset=30, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=7, argval='Hello world!', argrepr="'Hello world!'", offset=32, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=7, argval=7, argrepr='7 positional, 0 keyword pair', offset=34, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=36, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=2, argval='f', argrepr='f', offset=38, starts_line=8, is_jump_target=False),
  Instruction(opname='RETURN_VALUE', opcode=83, arg=0, argval=0, argrepr='', offset=40, starts_line=None, is_jump_target=False),
]

expected_opinfo_f = [
  Instruction(opname='LOAD_CONST', opcode=100, arg=1, argval=5, argrepr='5', offset=0, starts_line=3, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=2, argval=6, argrepr='6', offset=2, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CLOSURE', opcode=135, arg=2, argval='a', argrepr='a', offset=4, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CLOSURE', opcode=135, arg=3, argval='b', argrepr='b', offset=6, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CLOSURE', opcode=135, arg=0, argval='c', argrepr='c', offset=8, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CLOSURE', opcode=135, arg=1, argval='d', argrepr='d', offset=10, starts_line=None, is_jump_target=False),
  Instruction(opname='BUILD_TUPLE', opcode=102, arg=4, argval=4, argrepr='', offset=12, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=3, argval=code_object_inner, argrepr=repr(code_object_inner), offset=14, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=4, argval='outer.<locals>.f.<locals>.inner', argrepr="'outer.<locals>.f.<locals>.inner'", offset=16, starts_line=None, is_jump_target=False),
  Instruction(opname='MAKE_CLOSURE', opcode=134, arg=2, argval=2, argrepr='', offset=18, starts_line=None, is_jump_target=False),
  Instruction(opname='STORE_FAST', opcode=125, arg=2, argval='inner', argrepr='inner', offset=20, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=0, argval='print', argrepr='print', offset=22, starts_line=5, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=2, argval='a', argrepr='a', offset=24, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=3, argval='b', argrepr='b', offset=26, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=0, argval='c', argrepr='c', offset=28, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=1, argval='d', argrepr='d', offset=30, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=4, argval=4, argrepr='4 positional, 0 keyword pair', offset=32, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=34, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=2, argval='inner', argrepr='inner', offset=36, starts_line=6, is_jump_target=False),
  Instruction(opname='RETURN_VALUE', opcode=83, arg=0, argval=0, argrepr='', offset=38, starts_line=None, is_jump_target=False),
]

expected_opinfo_inner = [
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=0, argval='print', argrepr='print', offset=0, starts_line=4, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=0, argval='a', argrepr='a', offset=2, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=1, argval='b', argrepr='b', offset=4, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=2, argval='c', argrepr='c', offset=6, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_DEREF', opcode=136, arg=3, argval='d', argrepr='d', offset=8, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='e', argrepr='e', offset=10, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=1, argval='f', argrepr='f', offset=12, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=6, argval=6, argrepr='6 positional, 0 keyword pair', offset=14, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=16, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=0, argval=None, argrepr='None', offset=18, starts_line=None, is_jump_target=False),
  Instruction(opname='RETURN_VALUE', opcode=83, arg=0, argval=0, argrepr='', offset=20, starts_line=None, is_jump_target=False),
]

expected_opinfo_jumpy = [
  Instruction(opname='SETUP_LOOP', opcode=120, arg=52, argval=52, argrepr='to 52', offset=0, starts_line=3, is_jump_target=False),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=0, argval='range', argrepr='range', offset=2, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=1, argval=10, argrepr='10', offset=4, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=1, argval=1, argrepr='1 positional, 0 keyword pair', offset=6, starts_line=None, is_jump_target=False),
  Instruction(opname='GET_ITER', opcode=68, arg=0, argval=0, argrepr='', offset=8, starts_line=None, is_jump_target=False),
  Instruction(opname='FOR_ITER', opcode=93, arg=32, argval=42, argrepr='to 42', offset=10, starts_line=None, is_jump_target=True),
  Instruction(opname='STORE_FAST', opcode=125, arg=0, argval='i', argrepr='i', offset=12, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=1, argval='print', argrepr='print', offset=14, starts_line=4, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='i', argrepr='i', offset=16, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=1, argval=1, argrepr='1 positional, 0 keyword pair', offset=18, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=20, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='i', argrepr='i', offset=22, starts_line=5, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=2, argval=4, argrepr='4', offset=24, starts_line=None, is_jump_target=False),
  Instruction(opname='COMPARE_OP', opcode=107, arg=0, argval='<', argrepr='<', offset=26, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_JUMP_IF_FALSE', opcode=114, arg=32, argval=32, argrepr='', offset=28, starts_line=None, is_jump_target=False),
  Instruction(opname='JUMP_ABSOLUTE', opcode=113, arg=10, argval=10, argrepr='', offset=30, starts_line=6, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='i', argrepr='i', offset=32, starts_line=7, is_jump_target=True),
  Instruction(opname='LOAD_CONST', opcode=100, arg=3, argval=6, argrepr='6', offset=34, starts_line=None, is_jump_target=False),
  Instruction(opname='COMPARE_OP', opcode=107, arg=4, argval='>', argrepr='>', offset=36, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_JUMP_IF_FALSE', opcode=114, arg=10, argval=10, argrepr='', offset=38, starts_line=None, is_jump_target=False),
  Instruction(opname='BREAK_LOOP', opcode=80, arg=0, argval=0, argrepr='', offset=40, starts_line=8, is_jump_target=False),
  Instruction(opname='JUMP_ABSOLUTE', opcode=113, arg=10, argval=10, argrepr='', offset=42, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_BLOCK', opcode=87, arg=0, argval=0, argrepr='', offset=44, starts_line=None, is_jump_target=True),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=1, argval='print', argrepr='print', offset=46, starts_line=10, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=4, argval='I can haz else clause?', argrepr="'I can haz else clause?'", offset=48, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=1, argval=1, argrepr='1 positional, 0 keyword pair', offset=50, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=52, starts_line=None, is_jump_target=False),
  Instruction(opname='SETUP_LOOP', opcode=120, arg=68, argval=142, argrepr='to 142', offset=54, starts_line=11, is_jump_target=True),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='i', argrepr='i', offset=56, starts_line=None, is_jump_target=True),
  Instruction(opname='POP_JUMP_IF_FALSE', opcode=114, arg=131, argval=131, argrepr='', offset=58, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=1, argval='print', argrepr='print', offset=60, starts_line=12, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='i', argrepr='i', offset=62, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=1, argval=1, argrepr='1 positional, 0 keyword pair', offset=64, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=66, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='i', argrepr='i', offset=68, starts_line=13, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=5, argval=1, argrepr='1', offset=70, starts_line=None, is_jump_target=False),
  Instruction(opname='INPLACE_SUBTRACT', opcode=56, arg=0, argval=0, argrepr='', offset=72, starts_line=None, is_jump_target=False),
  Instruction(opname='STORE_FAST', opcode=125, arg=0, argval='i', argrepr='i', offset=74, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='i', argrepr='i', offset=76, starts_line=14, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=3, argval=6, argrepr='6', offset=78, starts_line=None, is_jump_target=False),
  Instruction(opname='COMPARE_OP', opcode=107, arg=4, argval='>', argrepr='>', offset=80, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_JUMP_IF_FALSE', opcode=114, arg=115, argval=115, argrepr='', offset=82, starts_line=None, is_jump_target=False),
  Instruction(opname='JUMP_ABSOLUTE', opcode=113, arg=74, argval=74, argrepr='', offset=84, starts_line=15, is_jump_target=False),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='i', argrepr='i', offset=86, starts_line=16, is_jump_target=True),
  Instruction(opname='LOAD_CONST', opcode=100, arg=2, argval=4, argrepr='4', offset=88, starts_line=None, is_jump_target=False),
  Instruction(opname='COMPARE_OP', opcode=107, arg=0, argval='<', argrepr='<', offset=90, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_JUMP_IF_FALSE', opcode=114, arg=74, argval=74, argrepr='', offset=92, starts_line=None, is_jump_target=False),
  Instruction(opname='BREAK_LOOP', opcode=80, arg=0, argval=0, argrepr='', offset=94, starts_line=17, is_jump_target=False),
  Instruction(opname='JUMP_ABSOLUTE', opcode=113, arg=74, argval=74, argrepr='', offset=96, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_BLOCK', opcode=87, arg=0, argval=0, argrepr='', offset=98, starts_line=None, is_jump_target=True),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=1, argval='print', argrepr='print', offset=100, starts_line=19, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=6, argval='Who let lolcatz into this test suite?', argrepr="'Who let lolcatz into this test suite?'", offset=102, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=1, argval=1, argrepr='1 positional, 0 keyword pair', offset=104, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=106, starts_line=None, is_jump_target=False),
  Instruction(opname='SETUP_FINALLY', opcode=122, arg=73, argval=218, argrepr='to 218', offset=108, starts_line=20, is_jump_target=True),
  Instruction(opname='SETUP_EXCEPT', opcode=121, arg=12, argval=160, argrepr='to 160', offset=110, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=5, argval=1, argrepr='1', offset=112, starts_line=21, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=7, argval=0, argrepr='0', offset=114, starts_line=None, is_jump_target=False),
  Instruction(opname='BINARY_TRUE_DIVIDE', opcode=27, arg=0, argval=0, argrepr='', offset=116, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=118, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_BLOCK', opcode=87, arg=0, argval=0, argrepr='', offset=120, starts_line=None, is_jump_target=False),
  Instruction(opname='JUMP_FORWARD', opcode=110, arg=28, argval=188, argrepr='to 188', offset=122, starts_line=None, is_jump_target=False),
  Instruction(opname='DUP_TOP', opcode=4, arg=0, argval=0, argrepr='', offset=126, starts_line=22, is_jump_target=True),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=2, argval='ZeroDivisionError', argrepr='ZeroDivisionError', offset=128, starts_line=None, is_jump_target=False),
  Instruction(opname='COMPARE_OP', opcode=107, arg=10, argval='exception match', argrepr='exception match', offset=130, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_JUMP_IF_FALSE', opcode=114, arg=187, argval=187, argrepr='', offset=132, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=136, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=138, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=140, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=1, argval='print', argrepr='print', offset=142, starts_line=23, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=8, argval='Here we go, here we go, here we go...', argrepr="'Here we go, here we go, here we go...'", offset=144, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=1, argval=1, argrepr='1 positional, 0 keyword pair', offset=146, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=148, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_EXCEPT', opcode=89, arg=0, argval=0, argrepr='', offset=150, starts_line=None, is_jump_target=False),
  Instruction(opname='JUMP_FORWARD', opcode=110, arg=27, argval=214, argrepr='to 214', offset=152, starts_line=None, is_jump_target=False),
  Instruction(opname='END_FINALLY', opcode=88, arg=0, argval=0, argrepr='', offset=154, starts_line=None, is_jump_target=True),
  Instruction(opname='LOAD_FAST', opcode=124, arg=0, argval='i', argrepr='i', offset=156, starts_line=25, is_jump_target=True),
  Instruction(opname='SETUP_WITH', opcode=143, arg=17, argval=211, argrepr='to 211', offset=158, starts_line=None, is_jump_target=False),
  Instruction(opname='STORE_FAST', opcode=125, arg=1, argval='dodgy', argrepr='dodgy', offset=160, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=1, argval='print', argrepr='print', offset=162, starts_line=26, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=9, argval='Never reach this', argrepr="'Never reach this'", offset=164, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=1, argval=1, argrepr='1 positional, 0 keyword pair', offset=166, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=168, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_BLOCK', opcode=87, arg=0, argval=0, argrepr='', offset=170, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=0, argval=None, argrepr='None', offset=180, starts_line=None, is_jump_target=False),
  Instruction(opname='WITH_CLEANUP_START', opcode=81, arg=0, argval=0, argrepr='', offset=182, starts_line=None, is_jump_target=True),
  Instruction(opname='WITH_CLEANUP_FINISH', opcode=82, arg=0, argval=0, argrepr='', offset=184, starts_line=None, is_jump_target=False),
  Instruction(opname='END_FINALLY', opcode=88, arg=0, argval=0, argrepr='', offset=186, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_BLOCK', opcode=87, arg=0, argval=0, argrepr='', offset=188, starts_line=None, is_jump_target=True),
  Instruction(opname='LOAD_CONST', opcode=100, arg=0, argval=None, argrepr='None', offset=190, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_GLOBAL', opcode=116, arg=1, argval='print', argrepr='print', offset=192, starts_line=28, is_jump_target=True),
  Instruction(opname='LOAD_CONST', opcode=100, arg=10, argval="OK, now we're done", argrepr='"OK, now we\'re done"', offset=194, starts_line=None, is_jump_target=False),
  Instruction(opname='CALL_FUNCTION', opcode=131, arg=1, argval=1, argrepr='1 positional, 0 keyword pair', offset=196, starts_line=None, is_jump_target=False),
  Instruction(opname='POP_TOP', opcode=1, arg=0, argval=0, argrepr='', offset=198, starts_line=None, is_jump_target=False),
  Instruction(opname='END_FINALLY', opcode=88, arg=0, argval=0, argrepr='', offset=200, starts_line=None, is_jump_target=False),
  Instruction(opname='LOAD_CONST', opcode=100, arg=0, argval=None, argrepr='None', offset=202, starts_line=None, is_jump_target=False),
  Instruction(opname='RETURN_VALUE', opcode=83, arg=0, argval=0, argrepr='', offset=204, starts_line=None, is_jump_target=False),
]

# One last piece of inspect fodder to check the default line number handling
def simple(): pass
expected_opinfo_simple = [
  Instruction(opname='LOAD_CONST', opcode=100, arg=0, argval=None, argrepr='None', offset=0, starts_line=simple.__code__.co_firstlineno, is_jump_target=False),
  Instruction(opname='RETURN_VALUE', opcode=83, arg=0, argval=0, argrepr='', offset=2, starts_line=None, is_jump_target=False)
]


class InstructionTests(BytecodeTestCase):

    def test_default_first_line(self):
        actual = dis.get_instructions(simple)
        self.assertEqual(list(actual), expected_opinfo_simple)

    def test_first_line_set_to_None(self):
        actual = dis.get_instructions(simple, first_line=None)
        self.assertEqual(list(actual), expected_opinfo_simple)

    def test_outer(self):
        actual = dis.get_instructions(outer, first_line=expected_outer_line)
        self.assertEqual(list(actual), expected_opinfo_outer)

    def test_nested(self):
        with captured_stdout():
            f = outer()
        actual = dis.get_instructions(f, first_line=expected_f_line)
        self.assertEqual(list(actual), expected_opinfo_f)

    def test_doubly_nested(self):
        with captured_stdout():
            inner = outer()()
        actual = dis.get_instructions(inner, first_line=expected_inner_line)
        self.assertEqual(list(actual), expected_opinfo_inner)

    def test_jumpy(self):
        actual = dis.get_instructions(jumpy, first_line=expected_jumpy_line)
        self.assertEqual(list(actual), expected_opinfo_jumpy)

# get_instructions has its own tests above, so can rely on it to validate
# the object oriented API
class BytecodeTests(unittest.TestCase):
    def test_instantiation(self):
        # Test with function, method, code string and code object
        for obj in [_f, _C(1).__init__, "a=1", _f.__code__]:
            with self.subTest(obj=obj):
                b = dis.Bytecode(obj)
                self.assertIsInstance(b.codeobj, types.CodeType)

        self.assertRaises(TypeError, dis.Bytecode, object())

    def test_iteration(self):
        for obj in [_f, _C(1).__init__, "a=1", _f.__code__]:
            with self.subTest(obj=obj):
                via_object = list(dis.Bytecode(obj))
                via_generator = list(dis.get_instructions(obj))
                self.assertEqual(via_object, via_generator)

    def test_explicit_first_line(self):
        actual = dis.Bytecode(outer, first_line=expected_outer_line)
        self.assertEqual(list(actual), expected_opinfo_outer)

    def test_source_line_in_disassembly(self):
        # Use the line in the source code
        actual = dis.Bytecode(simple).dis()[:3]
        expected = "{:>3}".format(simple.__code__.co_firstlineno)
        self.assertEqual(actual, expected)
        # Use an explicit first line number
        actual = dis.Bytecode(simple, first_line=350).dis()[:3]
        self.assertEqual(actual, "350")

    def test_info(self):
        self.maxDiff = 1000
        for x, expected in CodeInfoTests.test_pairs:
            b = dis.Bytecode(x)
            self.assertRegex(b.info(), expected)

    def test_disassembled(self):
        actual = dis.Bytecode(_f).dis()
        self.assertEqual(actual, dis_f)

    def test_from_traceback(self):
        tb = get_tb()
        b = dis.Bytecode.from_traceback(tb)
        while tb.tb_next: tb = tb.tb_next

        self.assertEqual(b.current_offset, tb.tb_lasti)

    def test_from_traceback_dis(self):
        tb = get_tb()
        b = dis.Bytecode.from_traceback(tb)
        self.assertEqual(b.dis(), dis_traceback)

if __name__ == "__main__":
    unittest.main()
