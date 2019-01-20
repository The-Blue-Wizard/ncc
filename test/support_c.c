#include "support.h"

void
puts(const char *s)
{
    while (*s) putc(*s++);
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
