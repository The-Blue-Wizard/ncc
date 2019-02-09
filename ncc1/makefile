HDRS=ncc1.h token.h symbol.h type.h tree.h block.h reg.h peep.h
OBJS=ncc1.o lex.o symbol.o type.o decl.o init.o stmt.o block.o \
	opt.o reg.o tree.o output.o peep.o gen.o 
CFLAGS=

ncc1: $(OBJS)
	$(CC) $(CFLAGS) -o ncc1 $(OBJS) 

%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c $<

clean::
	rm -f *.o ncc1
