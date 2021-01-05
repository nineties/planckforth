#!/usr/bin/env python3
# planck - 
# Copyright (C) 2021 nineties
#

import os
import sys
import operator

MEMORY_SIZE = 0x20000
STACK_SIZE = 0x400
RSTACK_SIZE = 0x400
HERE_CELL = 0
LATEST_CELL = 4

memory = [0]*(MEMORY_SIZE>>2)

sp = MEMORY_SIZE>>2
rp = (MEMORY_SIZE - STACK_SIZE)>>2
ip = 0
np = 0

def aligned(n):
    return (n + 4 - 1) & ~(4 - 1)

def align():
    write(HERE_CELL, aligned(read(HERE_CELL)))

def read(addr):
    return memory[addr>>2]

def write(addr, v):
    memory[addr>>2] = v

def comma(v):
    here = read(HERE_CELL)
    write(here, v)
    write(HERE_CELL, here + 4)

def read_byte(addr):
    i = addr>>2
    m = (addr&0x3)*8
    v = memory[i]
    return (v >> m) & 0xff

def write_byte(addr, c):
    i = addr>>2
    m = (addr&0x3)*8
    v = memory[i]
    memory[i] = (v & ~(0xff << m)) | (c&0xff) << m

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
        n = read_byte(it + 4)
        C = chr(read_byte(it + 4 + 1))
        if (c == C and n == 1):
            return it + 2*4
        it = read(it)
    raise Exception('Unknown word: {}'.format(c))

def push(v):
    global sp
    sp -= 4
    write(sp, v)

def pop():
    global sp
    v = read(sp)
    sp += 4
    return v

def rpush(v):
    global rp
    rp -= 4
    write(rp, v)

def rpop():
    global rp
    v = read(rp)
    rp += 4
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
    return read(np), np + 4

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

write(HERE_CELL, 2*4)
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
    return next(ip + 4)
DOCOL_ID = add_operator('', docol)
add_simple_operator('Q', lambda: exit(0))
add_simple_operator('C', lambda: push(4))
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
add_operator('j', lambda ip,np: next(np + read(np)))
add_operator('J', lambda ip,np: next(np + (4 if pop() else read(np))))
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
    return next(np + 4)
add_operator('L', lit)
def litstring(ip, np):
    push(np + 4)
    return next(aligned(np + 4 + read(np)))
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

SUCCESS = 0
ALLOCATE_ERROR = -59
CLOSE_FILE_ERROR = -62
OPEN_FILE_ERROR = -69
READ_FILE_ERROR = -70
WRITE_FILE_ERROR = -75
def openfile():
    flag = pop()
    name = read_string(pop())
    fd = os.open(name, flag)
    push(fd)
    push(SUCCESS if (fd >= 0) else OPEN_FILE_ERROR)
def closefile():
    fd = pop()
    os.close(fd)
    push(SUCCESS if (fd >= 0) else CLOSE_FILE_ERROR)
def readfile():
    fd = pop()
    size = pop()
    addr = pop()
    s = os.read(fd, size)
    write_string(addr, s)
    push(len(s))
    push(SUCCESS if (len(s) > 0) else READ_FILE_ERROR)
def writefile():
    fd = pop()
    size = pop()
    addr = pop()
    n = os.write(fd, read_bytes(addr, size))
    push(SUCCESS if (n == size) else WRITE_FILE_ERROR)
add_simple_operator('(open-file)', openfile)
add_simple_operator('(close-file)', closefile)
add_simple_operator('(write-file)', writefile)
add_simple_operator('(read-file)', readfile)
def allocate():
    size = pop()
    n = (size + 4 - 1) // 4
    addr = len(memory)*4
    mem.extend([0]*n)
    push(addr)
    push(SUCCESS)
add_simple_operator('allocate', allocate)

start = read(HERE_CELL)
comma(find('k'))
comma(find('f'))
comma(find('x'))
comma(find('j'))
comma(-4*4)

ip, np = next(start)
while True:
    ip, np = operators[read(ip)](ip, np)
