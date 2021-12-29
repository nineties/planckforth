#!/usr/bin/env python3
# planck - 
# Copyright (C) 2021 nineties
#

import os
import sys
import operator
import array
import ctypes
import platform

RUNTIME_NAME = "Python {}".format(platform.python_version())
COPYRIGHT = "Copyright (c) 2021 Koichi Nakamura <koichi@idein.jp>"

VERSION = "{}:{}".format(RUNTIME_NAME, COPYRIGHT)

MEMORY_SIZE = 0x40000

memory = bytearray(MEMORY_SIZE)
CELL = 4

STACK_SIZE = 0x400
RSTACK_SIZE = 0x400

HERE_CELL = 0
LATEST_CELL = CELL

sp = MEMORY_SIZE
rp = MEMORY_SIZE - STACK_SIZE
ip = 0
np = 0

ALIGN_MASK = ~(CELL - 1)
def aligned(n):
    return (n + CELL - 1) & ALIGN_MASK

def align():
    write(HERE_CELL, aligned(read(HERE_CELL)))

def read(addr, signed=False):
    return int.from_bytes(memory[addr:addr+CELL], 'little', signed=signed)

def write(addr, v):
    memory[addr:addr+CELL] = ctypes.c_uint32(v).value.to_bytes(CELL, 'little')

def comma(v, signed=False):
    here = read(HERE_CELL)
    write(here, v)
    write(HERE_CELL, here + CELL)

def read_byte(addr):
    return memory[addr]

def write_byte(addr, c):
    memory[addr] = c

def comma_byte(v):
    here = read(HERE_CELL)
    write_byte(here, v)
    write(HERE_CELL, here + 1)

def comma_string(s):
    for c in s:
        comma_byte(ord(c))
    comma_byte(0)

def write_string(addr, s):
    for c in s:
        write_byte(addr, c)
        addr += 1

def read_string(addr):
    s = ""
    while True:
        c = read_byte(addr)
        if c == 0: break
        s += chr(c)
        addr += 1
    return s

def read_bytes(addr, n):
    data = []
    for i in range(n):
        data.append(read_byte(addr))
        addr += 1
    return bytes(data)

def find(c):
    it = read(LATEST_CELL)
    while it != 0:
        n = read_byte(it + CELL)
        C = chr(read_byte(it + CELL + 1))
        if (c == C and n == 1):
            return it + 2*CELL
        it = read(it)
    raise Exception('Unknown word: {}'.format(c))

def push(v):
    global sp
    sp -= CELL
    write(sp, v)

def pop():
    global sp
    v = read(sp, signed=True)
    sp += CELL
    return v

def rpush(v):
    global rp
    rp -= CELL
    write(rp, v)

def rpop():
    global rp
    v = read(rp, signed=True)
    rp += CELL
    return v

operators = []
def add_operator(name, func):
    funcid = len(operators)
    here = read(HERE_CELL)
    latest = read(LATEST_CELL)
    write(LATEST_CELL, here)

    comma(latest)
    comma_byte(len(name))
    comma_string(name)
    align()
    comma(funcid)

    operators.append(func)
    return funcid

def next(np):
    return read(np), np + CELL

def add_simple_operator(name, func):
    def func_(ip, np):
        func()
        return next(np)
    return add_operator(name, func_)

def add_uint_operator(name, op):
    def func():
        b = ctypes.c_uint(pop()).value
        a = ctypes.c_uint(pop()).value
        push(op(a, b))
    return add_simple_operator(name, func)
def add_int_operator(name, op):
    def func():
        b = pop()
        a = pop()
        push(op(a, b))
    return add_simple_operator(name, func)

write(HERE_CELL, 2*CELL)
write(LATEST_CELL, 0)

# Store command line arguments
argv_addrs = []
for arg in sys.argv:
    argv_addrs.append(read(HERE_CELL))
    comma_string(arg)
align()
ARGV_ADDR = read(HERE_CELL)
for addr in argv_addrs:
    comma(addr)

# Version String
VERSION_ADDR = read(HERE_CELL)
comma_string(VERSION)
align()

def docol(ip, np):
    rpush(np)
    return next(ip + CELL)
DOCOL_ID = add_operator('', docol)
add_simple_operator('Q', lambda: exit(pop()))
add_simple_operator('C', lambda: push(CELL))
add_simple_operator('h', lambda: push(HERE_CELL))
add_simple_operator('l', lambda: push(LATEST_CELL))
def key():
    c = sys.stdin.read(1)
    if c:
        push(ord(c))
    else:
        exit(0)
add_simple_operator('k', key)
add_simple_operator('t', lambda: sys.stdout.write(chr(pop())))
add_operator('j', lambda ip,np: next(np + read(np, signed=True)))
add_operator('J', lambda ip,np: next(np + (CELL if pop() else read(np, signed=True))))
add_simple_operator('f', lambda: push(find(chr(pop()))))
add_operator('x', lambda ip,np: (pop(), np))
add_simple_operator('@', lambda: push(read(pop(), signed=True)))

# NB: Python evaluates expressions from left to right
# https://docs.python.org/3/reference/expressions.html#evaluation-order
add_simple_operator('!', lambda: write(pop(), pop()))
add_simple_operator('?', lambda: push(read_byte(pop())))
add_simple_operator('$', lambda: write_byte(pop(), pop()))
add_simple_operator('d', lambda: push(sp))
def set_sp():
    global sp
    sp = pop()
add_simple_operator('D', set_sp)
add_simple_operator('r', lambda: push(rp))
def set_rp():
    global rp
    rp = pop()
add_simple_operator('R', set_rp)
add_simple_operator('i', lambda: push(DOCOL_ID))
add_operator('e', lambda ip,np: next(rpop()))
def lit(ip, np):
    push(read(np, signed=True))
    return next(np + CELL)
add_operator('L', lit)
def litstring(ip, np):
    push(np + CELL)
    return next(np + CELL + read(np))
add_operator('S', litstring)
def divmod():
    b = ctypes.c_uint(pop()).value
    a = ctypes.c_uint(pop()).value
    push(a%b)
    push(a//b)
add_simple_operator('/', divmod)
add_int_operator('+', operator.add)
add_int_operator('-', operator.sub)
add_int_operator('*', operator.mul)
add_uint_operator('&', operator.and_)
add_uint_operator('|', operator.or_)
add_uint_operator('^', operator.xor)
add_int_operator('<', operator.lt)
add_uint_operator('u', operator.lt)
add_uint_operator('=', operator.eq)
add_uint_operator('(', operator.lshift)
add_uint_operator(')', operator.rshift)
add_int_operator('%', operator.rshift)
def argv():
    push(ARGV_ADDR)
    push(len(sys.argv))
add_simple_operator('v', argv)
add_simple_operator('V', lambda: push(VERSION_ADDR))

def openfile():
    flag = pop()
    name = read_string(pop())
    fd = os.open(name, flag)
    push(fd)
def closefile():
    fd = pop()
    os.close(fd)
    push(0)
def readfile():
    fd = pop()
    size = pop()
    addr = pop()
    s = os.read(fd, size)
    write_string(addr, s)
    push(len(s))
def writefile():
    fd = pop()
    size = pop()
    addr = pop()
    n = os.write(fd, read_bytes(addr, size))
    push(n)
add_simple_operator('(open)', openfile)
add_simple_operator('(close)', closefile)
add_simple_operator('(write)', writefile)
add_simple_operator('(read)', readfile)
def allocate():
    size = pop()
    addr = len(memory)
    memory.extend([0]*size)
    push(addr)
def free():
    pop()   # Bootstrap version do nothing
add_simple_operator('(allocate)', allocate)
add_simple_operator('(free)', free)

start = read(HERE_CELL)
comma(find('k'))
comma(find('f'))
comma(find('x'))
comma(find('j'))
comma(-4*CELL)

ip, np = next(start)
while True:
    ip, np = operators[read(ip)](ip, np)
