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
#include <ctype.h>

#include "escheme.h"

#define MAX_BUFFSIZE 2048

static tchar strbuf[MAX_BUFFSIZE];


/**
 * @brief open `name' with the read rights
 */
escm_input *
escm_input_fopen(const char *name)
{
    escm_input *f;

    assert(name != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = INPUT_FILE;

    f->d.file.fp = fopen(name, "r");
    if (!f->d.file.fp) {
        perror(name);
        free(f);
        f = NULL;
    } else {
        f->d.file.name = xstrdup(name);
        f->d.file.line = 1;
        f->managed = 0;
    }

    return f;
}

/**
 * @brief manage a file pointer
 */
escm_input *
escm_input_fmng(FILE *fp, const char *name)
{
    escm_input *f;

    assert(fp != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = INPUT_FILE;
    f->d.file.fp = fp;

    f->d.file.name = xstrdup(name);
    f->d.file.line = f->d.file.car = -1; /* signal that we can't know where we
                                            are in the stream */
    f->managed = 1;

    return f;
}

/**
 * @brief set the string as input
 */
escm_input *
escm_input_str(const tchar *str)
{
    escm_input *f;

    assert(str != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = INPUT_STR;

    f->d.str.str = tcsdup(str);
    f->d.str.cur = f->d.str.str;

    return f;
}

/**
 * @brief close the input and free it
 */
void
escm_input_close(escm_input *f)
{
    if (!f)
        return;

    if (f->type == INPUT_FILE) {
        if (!f->managed) {
            if (EOF == fclose(f->d.file.fp))
                perror("fclose");
        }
        free(f->d.file.name);
        free(f->d.file.ub);
    } else
        free(f->d.str.str);

    free(f);
}

/**
 * @brief return a text that end with one of the `end' chars
 */
tchar *
escm_input_gettext(escm_input *f, const tchar *end)
{
    size_t len = 0;
    tint c;

    assert(f != NULL);
    assert(end != NULL);

    c = escm_input_getc(f);
    while (c != TEOF && !tcschr(end, c)) {
        if (c == T('\\')) {
            c = escm_input_getc(f);
            switch (c) {
            case T('a'): c = T('\a'); break;
            case T('b'): c = T('\b'); break;
            case T('f'): c = T('\f'); break;
            case T('n'): c = T('\n'); break;
            case T('r'): c = T('\r'); break;
            case T('t'): c = T('\t'); break;
            case T('v'): c = T('\v'); break;
            case T('\\'): case T('"'): break;
            default:
                strbuf[len++] = T('\\');
                break; /* keep the new character */
            }
            strbuf[len++] = c;
        } else
            strbuf[len++] = c;
        c = escm_input_getc(f);
    }

    if (!f->end)
        escm_input_ungetc(f, c);
    strbuf[len] = T('\0');

    return tcsdup(strbuf);
}

/**
 * @brief get a string. Each character is passed to "fun" which must return 1
 * if the character is valid, 0 else (cf ctype.h)
 */
tchar *
escm_input_getstr_fun(escm_input *f, int (*fun)(tint), int casesens)
{
    size_t len = 0;
    tint c;

    assert(f != NULL);

    do {
        c = escm_input_getc(f);
        if (!casesens)
            c = tolower(c);
        strbuf[len++] = c;
    } while (c != TEOF && fun(c));

    if (!f->end)
        escm_input_ungetc(f, c);
    strbuf[len - 1] = T('\0');

    return tcsdup(strbuf);
}

/**
 * @brief rewind the input structure
 */
void
escm_input_rewind(escm_input *f)
{
    assert(f != NULL);

    if (f->type == INPUT_FILE) {
        rewind(f->d.file.fp);
        f->d.file.line = 0;
        f->d.file.car = 0;
    } else
        f->d.str.cur = f->d.str.str;

    f->end = 0;
}

/* print a brief resume of the input ("filename:line:car: " or ""str":car: ") */
void
escm_input_print(escm_input *f, escm_output *outp)
{
    assert(f != NULL);

    if (f->type == INPUT_FILE) {
        escm_printf(outp, T("%s:"), f->d.file.name);
        if (f->d.file.line == -1)
            escm_putc(outp, T(':'));
        else
            escm_printf(outp, T("%ld:"), f->d.file.line);
        if (f->d.file.car == -1)
            escm_printf(outp, T(": "));
        else
            escm_printf(outp, T("%ld: "), f->d.file.car);
    } else {
        escm_printf(outp, T("\"%ls\":%d: "), f->d.str.str,
                    f->d.str.cur - f->d.str.str);
    }
}

/**
 * @brief return the next char in the stream
 */
tint
escm_input_getc(escm_input *f)
{
    tint c;

    assert(f != NULL);

    if (f->end)
        return TEOF;

    if (f->type == INPUT_FILE) {
        if (f->d.file.un > 0)
            c = f->d.file.ub[--f->d.file.un];
        else
            c = fgettc(f->d.file.fp);

        if (c == TEOF)
            f->end = 1;
        else if (c == T('\n')) {
            if (f->d.file.line != -1)
                f->d.file.line++;
            f->d.file.car = 0;
        } else {
            if (f->d.file.car != -1)
                f->d.file.car++;
        }
    } else {
        if (*f->d.str.cur == T('\0'))
            f->end = 1, c = TEOF;
        else
            c = *f->d.str.cur++;
    }

    return c;
}

tint
escm_input_peek(escm_input *f)
{
    tint c;

    assert(f != NULL);

    if (f->end)
        return TEOF;

    if (f->type == INPUT_FILE) {
        if (f->d.file.un > 0)
            return f->d.file.ub[f->d.file.un - 1];

        c = fgettc(f->d.file.fp);

        if (f->d.file.usize <= f->d.file.un) {
            f->d.file.usize += 2;
            f->d.file.ub = xrealloc(f->d.file.ub,
                                    f->d.file.usize * sizeof *f->d.file.ub);
        }
        f->d.file.ub[f->d.file.un++] = c;
    } else
        c = *f->d.str.cur;

    return c;
}

/**
 * @brief put a character back in the input
 */
void
escm_input_ungetc(escm_input *f, tint c)
{
    assert(f != NULL);

    if (f->type == INPUT_FILE) {
        if (f->d.file.car != -1) {
            if (f->d.file.car > 0)
                f->d.file.car--;
            else if (f->d.file.car == 0 && c == T('\n') &&
                     f->d.file.line != -1)
                f->d.file.line--;
        }

        if (f->d.file.usize <= f->d.file.un) {
            f->d.file.usize += 2;
            f->d.file.ub = xrealloc(f->d.file.ub,
                                    f->d.file.usize * sizeof *f->d.file.ub);
        }
        f->d.file.ub[f->d.file.un++] = c;
    } else {
        if (f->d.str.cur > f->d.str.str)
            f->d.str.cur--;
    }

    f->end = 0;
}
