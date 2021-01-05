#!/usr/bin/env python3
# planck - 
# Copyright (C) 2021 nineties
#

import sys
import operator
CELL = 4
MEMORY_SIZE = 0x08000000
STACK_SIZE = 0x400
RSTACK_SIZE = 0x400
HERE_CELL = 0
LATEST_CELL = CELL

memory = bytearray(MEMORY_SIZE)
sp = MEMORY_SIZE
rp = MEMORY_SIZE - STACK_SIZE
ip = 0
np = 0

def aligned(n):
    return (n + CELL - 1) & ~(CELL - 1)

def align():
    write(HERE_CELL, aligned(read(HERE_CELL)))

def read(addr):
    return int.from_bytes(
            memory[addr:addr+CELL], 'little', signed=True)

def write(addr, v):
    memory[addr:addr+CELL] = v.to_bytes(4, 'little', signed=True)

def comma(v):
    here = read(HERE_CELL)
    write(here, v)
    write(HERE_CELL, here + CELL)

def read_byte(addr):
    return memory[addr]

def write_byte(addr, v):
    memory[addr] = v

def comma_byte(v):
    here = read(HERE_CELL)
    memory[here] = v
    write(HERE_CELL, here + 1)

def comma_string(s):
    n = len(s)
    here = read(HERE_CELL)
    memory[here:here+n+1] = bytes(s+'\0', 'ascii')
    write(HERE_CELL, here+n+1)

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
    v = read(sp)
    sp += CELL
    return v

def rpush(v):
    global rp
    rp -= CELL
    write(rp, v)

def rpop():
    global rp
    v = read(rp)
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

def add_binary_operator(name, op):
    def func():
        b = pop()
        push(op(pop(), b))
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

def docol(ip, np):
    rpush(np)
    return next(ip + CELL)
DOCOL_ID = add_operator('', docol)
add_simple_operator('Q', lambda: exit(0))
add_simple_operator('C', lambda: push(CELL))
add_simple_operator('h', lambda: push(HERE_CELL))
add_simple_operator('l', lambda: push(LATEST_CELL))
add_simple_operator('k', lambda: push(ord(sys.stdin.read(1))))
add_simple_operator('t', lambda: sys.stdout.write(chr(pop())))
add_operator('j', lambda ip,np: next(np + read(np)))
add_operator('J', lambda ip,np: next(np + (CELL if pop() else read(np))))
add_simple_operator('f', lambda: push(find(chr(pop()))))
add_operator('x', lambda ip,np: (pop(), np))
add_simple_operator('@', lambda: push(read(pop())))

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
    push(read(np))
    return next(np + CELL)
add_operator('L', lit)
def litstring(ip, np):
    push(np + CELL)
    return next(aligned(np + CELL + read(np)))
add_operator('S', litstring)
add_binary_operator('+', operator.add)
add_binary_operator('-', operator.sub)
add_binary_operator('*', operator.mul)
add_binary_operator('/', operator.floordiv)
add_binary_operator('%', operator.mod)
add_binary_operator('&', operator.and_)
add_binary_operator('|', operator.or_)
add_binary_operator('^', operator.xor)
add_binary_operator('<', operator.lt)
add_binary_operator('=', operator.eq)
def argv():
    push(ARGV_ADDR)
    push(len(sys.argv))
add_simple_operator('v', argv)

start = read(HERE_CELL)
comma(find('k'))
comma(find('f'))
comma(find('x'))
comma(find('j'))
comma(-4*CELL)

ip, np = next(start)
while True:
    ip, np = operators[read(ip)](ip, np)
