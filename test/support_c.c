#include "support.h"

void
puts(const char *s)
{
    while (*s) putc(*s++);
    putc('\n');
}

void 
putb(int b)
{
    static char xdigits[] = "0123456789ABCDEF";

    putc(xdigits[(b >> 4) & 0x0F]);
    putc(xdigits[b & 0x0F]);
}

void
putobj(void * obj, int length)
{
    unsigned char * b = obj;

    while (length--) putb(*b++);
    putc('\n');
}

void
putn(long l)
{
    char buffer[20];    /* 2^64 = 20 decimal digits max */
    int  i = 0;
    unsigned long ul;

    if (l < 0) {
        putc('-');
        l = -l;
    }

    ul = l;

    do {
        buffer[i++] = (ul % 10) + '0';
        ul /= 10;
    } while (ul);

    while (i) putc(buffer[--i]);
    putc('\n');
}
