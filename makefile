CC=gcc
CFLAGS=-g

all:: ncc nld nobj nexec
	make CC="$(CC)" CFLAGS="$(CFLAGS)" -C ncpp 
	make CC="$(CC)" CFLAGS="$(CFLAGS)" -C ncc1
	make CC="$(CC)" CFLAGS="$(CFLAGS)" -C nas

ncc: ncc.c
nld: nld.c
nobj: nobj.c
nexec: nexec.c

install:: all
	mkdir -p ~/bin
	cp ncc nld nobj nexec ncpp/ncpp ncc1/ncc1 nas/nas ~/bin

clean::
	rm -f nld ncc nobj nexec
	make -C ncpp clean
	make -C ncc1 clean
	make -C nas clean
	rm -f test/*.test.* test/support_c.i test/support_c.s test/support.o

test:: all
	(cd test; ./run_tests.sh)
