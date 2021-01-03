# planck - 
# Copyright (C) 2021 nineties
#

import sys
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
pc = 0

def read(addr):
    return int.from_bytes(
            memory[addr:addr+CELL], 'little', signed=True)

def write(addr, v):
    memory[addr:addr+CELL] = v.to_bytes(4, 'little', signed=True)

def read_byte(addr):
    return memory[addr]

def write_byte(addr, v):
    memory[addr] = v

next_builtin_id = 1
def add_builtin(c):
    global next_builtin_id
    builtin_id = next_builtin_id
    next_builtin_id += 1

    here = read(HERE_CELL)
    latest = read(LATEST_CELL)
    write(here, latest)
    write_byte(here + CELL, 1)
    write_byte(here + CELL + 1, ord(c))
    write(here + 2*CELL, builtin_id)

    write(HERE_CELL, here + 3*CELL)
    write(LATEST_CELL, here)
    return builtin_id

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

write(HERE_CELL, 2*CELL)
write(LATEST_CELL, 0)

DOCOL_    = 0
QUIT      = add_builtin('Q')
CELLSIZE  = add_builtin('C')
HERE      = add_builtin('h')
LATEST    = add_builtin('l')
KEY       = add_builtin('k')
EMIT      = add_builtin('t')
JUMP      = add_builtin('j')
JUMP0     = add_builtin('J')
FIND      = add_builtin('f')
EXECUTE   = add_builtin('x')
FETCH     = add_builtin('@')
STORE     = add_builtin('!')
CFETCH    = add_builtin('?')
CSTORE    = add_builtin('$')
DFETCH    = add_builtin('d')
DSTORE    = add_builtin('D')
RFETCH    = add_builtin('r')
RSTORE    = add_builtin('R')
DOCOL     = add_builtin('i')
EXIT      = add_builtin('e')
LIT       = add_builtin('L')
LITSTRING = add_builtin('S')
ADD       = add_builtin('+')
SUB       = add_builtin('-')
MUL       = add_builtin('*')
DIV       = add_builtin('/')
MOD       = add_builtin('%')
AND       = add_builtin('&')
OR        = add_builtin('|')
XOR       = add_builtin('^')
LESS      = add_builtin('<')
EQUAL     = add_builtin('=')

here = read(HERE_CELL)
write(here, find('k'))
write(here + CELL, find('f'))
write(here + 2*CELL, find('x'))
write(here + 3*CELL, find('j'))
write(here + 4*CELL, -4*CELL)
write(HERE_CELL, here + 5*CELL)

pc = here
ip = read(pc)
pc += CELL
while True:
    code = read(ip)
    if code == DOCOL_:
        rpush(pc)
        pc = ip + CELL
    elif code == QUIT:
        exit(0)
    elif code == CELLSIZE:
        push(CELL)
    elif code == HERE:
        push(HERE_CELL)
    elif code == LATEST:
        push(LATEST_CELL)
    elif code == KEY:
        push(ord(sys.stdin.read(1)))
    elif code == EMIT:
        sys.stdout.write(chr(pop()))
    elif code == JUMP:
        pc += read(pc)
    elif code == JUMP0:
        if pop() == 0:
            pc += read(pc)
        else:
            pc += CELL
    elif code == FIND:
        push(find(chr(pop())))
    elif code == EXECUTE:
        ip = pop()
        continue
    elif code == FETCH:
        push(read(pop()))
    elif code == STORE:
        addr = pop()
        write(addr, pop())
    elif code == CFETCH:
        push(read_byte(pop()))
    elif code == CSTORE:
        addr = pop()
        write_byte(addr, pop())
    elif code == DFETCH:
        push(sp)
    elif code == DSTORE:
        sp = pop()
    elif code == RFETCH:
        push(rp)
    elif code == RSTORE:
        rp = pop()
    elif code == DOCOL:
        push(DOCOL_)
    elif code == EXIT:
        pc = rpop()
    elif code == LIT:
        push(read(pc))
        pc += CELL
    elif code == LITSTRING:
        n = read(pc)
        push(pc + CELL)
        push(n)
        pc = (pc + 2*CELL + n - 1) & ~CELL
    elif code == ADD:
        b = pop()
        push(pop() + b)
    elif code == SUB:
        b = pop()
        push(pop() - b)
    elif code == MUL:
        b = pop()
        push(pop() * b)
    elif code == DIV:
        b = pop()
        push(pop() // b)
    elif code == MOD:
        b = pop()
        push(pop() % b)
    elif code == AND:
        b = pop()
        push(pop() & b)
    elif code == OR:
        b = pop()
        push(pop() | b)
    elif code == XOR:
        b = pop()
        push(pop() ^ b)
    elif code == LESS:
        b = pop()
        push(pop() < b)
    elif code == EQUAL:
        b = pop()
        push(pop() == b)
    else:
        raise Exception('Invalid Code Pointer: {}'.format(code))
    ip = read(pc)
    pc += CELL
