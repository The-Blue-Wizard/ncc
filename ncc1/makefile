HDRS=ncc1.h token.h symbol.h type.h tree.h block.h reg.h peep.h
OBJS=ncc1.o lex.o symbol.o type.o decl.o init.o stmt.o block.o \
	opt.o reg.o tree.o output.o peep.o gen.o 
CFLAGS=-Wmissing-prototypes -Wno-implicit-int -Wno-implicit-function-declaration -Wstrict-prototypes -g -Wold-style-definition -Wold-style-declaration

ncc1: $(OBJS)
	$(CC) $(CFLAGS) -o ncc1 $(OBJS) 

%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c $<

clean::
	rm -f *.o ncc1
