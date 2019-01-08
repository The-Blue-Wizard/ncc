/* Copyright (c) 2018 Charles E. Youse (charles@gnuless.org). 
   All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include "a.out.h"

void
error(const char * fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    fprintf(stderr, "nexec: ");
    vfprintf(stderr, fmt, args);
    fputc('\n', stderr);
    va_end(args);
    exit(1);
}

int
main(int argc, char * argv[])
{
    int         ( * entry ) (void);
    void         * base = 0;
    int            opt;
    char         * endptr;
    int            fd;
    struct exec    exec;
    int            length;
    
    while ((opt = getopt(argc, argv, "b:")) != -1) {
        switch (opt)
        {
        case 'b':
            if (base) error("only one base address (-b) please");
            errno = 0;
            base = (void *) strtoul(optarg, &endptr, 0);
            if (errno || *endptr) error("invalid base address (-b)");
            break;
        case '?':
            exit(1);
        }
    }

    if (!base) error("must specify base address (-b)");
    if ((argc - optind) != 1) error("missing path");
    fd = open(argv[optind], O_RDONLY);
    if (fd == -1) error("can't open '%s': %s\n", argv[optind], strerror(errno));
    if (read(fd, &exec, sizeof(exec)) != sizeof(exec)) error("can't read a.out header");
    if (exec.a_magic != A_MAGIC) error("invalid a.out header (magic)");
    length = exec.a_text;
    length += exec.a_data;
    length += exec.a_bss;
    if (mmap(base, length, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0) != base)
        error("can't map %d bytes at %p\n", length, base);
    memset(base, 0, length);    /* does mmap() guarantee this? */
    lseek(fd, 0, SEEK_SET);
    if (read(fd, base, exec.a_text + exec.a_data) != (exec.a_text + exec.a_data))
        error("can't read a.out: short read");
    entry = (int (*)(void)) (long) exec.a_entry;
    fprintf(stderr, "exit code = %d\n", entry());
    return 0;
}

