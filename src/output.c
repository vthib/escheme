/*
 * Copyright (c) 2007 Vincent "drexil" Thiberville <mahnmut@gmail.com>
 *
 * This file is part of Escheme. Escheme is free software; you can redistribute
 * it and/or modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of the License,
 * or (at your option) any later version.
 *
 * Escheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Escheme; If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

#include "escheme.h"
#include "config.h"

#ifdef ESCM_USE_UNICODE
# include <wchar.h>
# define C(char) L ## char
# define ESCM_PUTC fputwc
#else
# define C(char) char
# define ESCM_PUTC fputc
#endif

#define vscmpf(e, stream, format, next)                                 \
    {                                                                   \
        char *p;                                                        \
                                                                        \
        if (!(stream))                                                  \
            return;                                                     \
                                                                        \
        for (p = (char *) (format); *p != '\0'; p++) {                  \
            if (*p == '~') {                                            \
                p++;                                                    \
                switch (*p) {                                           \
                case '~':                                               \
                    escm_putc((stream), '~');                           \
                    break;                                              \
                case 'a':                                               \
                    escm_atom_print4((e), (next), (stream), 1);         \
                    break;                                              \
                case 's':                                               \
                    escm_atom_print4((e), (next), (stream), 0);         \
                    break;                                              \
                case 'n': case '%':                                     \
                    escm_putc((stream), '\n');                          \
                    break;                                              \
                default:                                                \
                    escm_putc((stream), '~');                           \
                    escm_putc((stream), *p);                            \
                    break;                                              \
                }                                                       \
            } else                                                      \
                escm_putc((stream), *p);                                \
        }                                                               \
    }

#define slashify(stream, str)                                   \
{                                                               \
    size_t i;                                                   \
                                                                \
    if (!stream)                                                \
        return;                                                 \
                                                                \
    for (i = 0; str[i] != '\0'; i++) {                          \
        switch (str[i]) {                                       \
        case '"':                                               \
        case '\\':                                              \
            escm_putc(stream, '\\');                            \
            escm_putc(stream, str[i]);                          \
            break;                                              \
        case '\a':                                              \
            escm_putc(stream, '\\'); escm_putc(stream, 'a');    \
            break;                                              \
        case '\b':                                              \
            escm_putc(stream, '\\'); escm_putc(stream, 'b');    \
            break;                                              \
        case '\f':                                              \
            escm_putc(stream, '\\'); escm_putc(stream, 'f');    \
            break;                                              \
        case '\n':                                              \
            escm_putc(stream, '\\'); escm_putc(stream, 'n');    \
            break;                                              \
        case '\r':                                              \
            escm_putc(stream, '\\'); escm_putc(stream, 'r');    \
            break;                                              \
        case '\t':                                              \
            escm_putc(stream, '\\'); escm_putc(stream, 't');    \
            break;                                              \
        case '\v':                                              \
            escm_putc(stream, '\\'); escm_putc(stream, 'v');    \
            break;                                              \
        default:                                                \
            escm_putc(stream, str[i]);                          \
            break;                                              \
        }                                                       \
    }                                                           \
}

/**
 * @brief open `name' with the read rights
 */
escm_output *
escm_output_fopen(const char *name)
{
    escm_output *f;

    assert(name != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = OUTPUT_FILE;

    f->d.file.fp = fopen(name, "w");
    if (!f->d.file.fp) {
        perror(name);
        free(f);
        f = NULL;
    } else {
        f->d.file.name = xstrdup(name);
    }

    return f;
}

/**
 * @brief manage a file pointer
 */
escm_output *
escm_output_fmng(FILE *fp, const char *name)
{
    escm_output *f;

    assert(fp != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = OUTPUT_FILE;
    f->d.file.fp = fp;
    f->d.file.name = xstrdup(name);

    return f;
}

/**
 * @brief set the string as output
 */
escm_output *
escm_output_str(void)
{
    escm_output *f;

    f = xcalloc(1, sizeof *f);
    f->type = OUTPUT_STR;

    f->d.str.str = xcalloc(5, sizeof *f->d.str.str);
    f->d.str.cur = f->d.str.str;
    f->d.str.maxlen = 5;

    return f;
}

escm_char *
escm_output_getstr(escm_output *o)
{
    assert(o != NULL);

    if (o->type == OUTPUT_FILE)
        return NULL;

    *o->d.str.cur = C('\0');
    return o->d.str.str;
}

void
escm_output_close(escm_output *f)
{
    if (!f)
        return;

    if (f->type == OUTPUT_FILE) {
        if (EOF == fclose(f->d.file.fp))
            perror("fclose");
        free(f->d.file.name);
    } else
        free(f->d.str.str);

    free(f);
}

void
escm_vprintf(escm_output *f, const char *format, va_list args)
{
    va_list va;

    va_copy(va, args);

    if (!f)
        return;

    if (f->type == OUTPUT_FILE)
        (void) vfprintf(f->d.file.fp, format, args);
    else {
        size_t offset;
        int write;
#ifdef ESCM_USE_UNICODE
        wchar_t *fmt;

        fmt = strtowcs(format);
#endif

        offset = f->d.str.cur - f->d.str.str;

        for (;;) {
            /* XXX: va_copy doesn't exist in C89, so we have only one chance
               to print the format in the string and we can't grow the string
               each time it failed. */
#ifndef ESCM_USE_C99
            f->d.str.maxlen += 80;
#else
            f->d.str.maxlen += 30;
#endif

            f->d.str.str = xrealloc(f->d.str.str, f->d.str.maxlen);
            f->d.str.cur = f->d.str.str + offset;

#ifdef ESCM_USE_UNICODE
# ifdef HAVE_VSNWPRINTF
            write = _vsnwprintf(f->d.str.cur, f->d.str.maxlen - offset, fmt, va);
# else
            write = vswprintf(f->d.str.cur, f->d.str.maxlen - offset, fmt, va);
# endif
#else
            write = vsnprintf(f->d.str.cur, f->d.str.maxlen - offset, format,
                              va);
#endif
            if ((size_t) write < f->d.str.maxlen - offset) {
                f->d.str.cur += write;
#ifdef ESCM_USE_UNICODE
                free(fmt);
#endif
                return;
            } else {
                va_end(args);
                va_copy(va, args);
            }
        }
    }
}

void
escm_printf(escm_output *stream, const char *format, ...)
{
    va_list args;

    va_start(args, format);

    escm_vprintf(stream, format, args);

    va_end(args);
}

void
escm_parse_print(escm_input *input, escm_output *stream, const char *format,
                 ...)
{
    va_list args;

    va_start(args, format);

    escm_input_print(input, stream);
    escm_vprintf(stream, format, args);

    va_end(args);
}

void
escm_scmpf(escm *e, escm_output *stream, const char *format, ...)
{
    va_list va;

    va_start(va, format);

    vscmpf(e, stream, format, va_arg(va, escm_atom *));

    va_end(va);
}

void
escm_scmpf2(escm *e, escm_output *stream, const char *format, escm_atom *args)
{
    vscmpf(e, stream, format, escm_cons_pop(e, &args));
}

void
escm_notice(escm *e, const char *format, ...)
{
    va_list va;

    va_start(va, format);

    vscmpf(e, e->output, format, va_arg(va, escm_atom *));

    va_end(va);
}

void
escm_warning(escm *e, const char *format, ...)
{
    va_list va;

    va_start(va, format);

    vscmpf(e, e->errp, format, va_arg(va, escm_atom *));

    va_end(va);
}

void
escm_error(escm *e, const char *format, ...)
{
    va_list va;

    va_start(va, format);

    vscmpf(e, e->errp, format, va_arg(va, escm_atom *));

    escm_print_backtrace(e, e->errp);

    va_end(va);
    e->err = 1;
}

void
escm_print_slashify(escm_output *stream, const char *str)
{
   slashify(stream, str);
}

#ifdef ESCM_USE_UNICODE
void
escm_print_wslashify(escm_output *stream, const wchar_t *str)
{
    slashify(stream, str);
}
#endif

void
escm_putc(escm_output *f, escm_int c)
{
    if (!f)
        return;

    if (f->type == OUTPUT_FILE) {
        if (ESCM_EOF == ESCM_PUTC(c, f->d.file.fp))
            fprintf(stderr, "fputc('%c') failed.\n", c);
    } else {
        size_t offset;

        *f->d.str.cur++ = c;

        offset = f->d.str.cur - f->d.str.str;

        if (offset >= f->d.str.maxlen) {
            f->d.str.maxlen += 5;
            f->d.str.str = xrealloc(f->d.str.str, f->d.str.maxlen);
            f->d.str.cur = f->d.str.str + offset;
        }
    }
}
