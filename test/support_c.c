#include "support.h"

void
puts(const char *s)
{
    while (*s) putc(*s++);
    putc('\n');
}
