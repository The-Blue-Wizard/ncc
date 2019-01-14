OBJS=ncpp.o input.o directive.o token.o macro.o vstring.o
CFLAGS=

ncpp: $(OBJS)
	$(CC) $(CFLAGS) -o ncpp $(OBJS)

clean::
	rm -f *.o ncpp
