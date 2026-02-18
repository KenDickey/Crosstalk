# Makefile for the UMB Scheme interpreter.
CC = gcc
CFLAGS = 
#CFLAGS =  -g -O   -pedantic -Wall

#CC = cc
#CFLAGS = -O 

global_h = portable.h object.h architecture.h number.h
num_h    = fixnum.h bignum.h rational.h real.h complex.h
objects = object.o primitive.o steering.o debug.o\
	  io.o compiler.o eval.o architecture.o number.o\
	  fixnum.o bignum.o rational.o real.o complex.o
libraries = -lm
program = scheme

.PHONY:		default tags print

default:	$(program)

$(program):	$(objects)
		$(CC) $(CFLAGS) -o $(program) $(objects) $(libraries)

$(objects):	$(global_h)

object.o:	eval.h primitive.h steering.h io.h compiler.h
steering.o:	eval.h primitive.h steering.h debug.h io.h
debug.o:	eval.h primitive.h steering.h io.h
architecture.o:	eval.h primitive.h steering.h io.h
io.o:		eval.h primitive.h steering.h io.h
compiler.o:	eval.h steering.h io.h
eval.o:		eval.h steering.h debug.h io.h
primitive.o:	eval.h primitive.h steering.h io.h
number.o:	eval.h steering.h io.h $(num_h)
fixnum.o:	eval.h steering.h io.h $(num_h)
bignum.o:	eval.h steering.h io.h $(num_h)
rational.o:	eval.h steering.h io.h $(num_h)
real.o:		eval.h steering.h io.h $(num_h)
complex.o:	eval.h steering.h io.h $(num_h)

tags:
		etags -t *.c *.h

print:
		ctags -t -x *.c *.h 2>/dev/null | fold > tag_defs
		lpr -Plp -p -JScheme Makefile tag_defs \
                portable.h \
		steering.h steering.c \
		debug.h debug.c \
                architecture.h architecture.c \
                io.h io.c \
		object.h object.c \
                compiler.h compiler.c \
		eval.h eval.c \
		primitive.h primitive.c \
                number.h  number.c \
		bignum.h bignum.c \
		real.h real.c \
		fixnum.h fixnum.c \
		rational.h rational.c \
		complex.h complex.c 
		/bin/rm -f tag_defs

nonums:
		lpr -Plp -p -JScheme Makefile \
                portable.h \
		steering.h steering.c \
		debug.h  debug.c \
		eval.h eval.c \
		primitive.h primitive.c  \
		object.h object.c \
                compiler.h compiler.c \
                architecture.h architecture.c \
                io.h io.c 

clean:
		rm -rf *.o a.out scheme
