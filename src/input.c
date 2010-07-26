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

static char strbuf[MAX_BUFFSIZE];


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
escm_input_str(const char *str)
{
    escm_input *f;

    assert(str != NULL);

    f = xcalloc(1, sizeof *f);
    f->type = INPUT_STR;

    f->d.str.str = xstrdup(str);
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
char *
escm_input_gettext(escm_input *f, const char *end)
{
    size_t len = 0;
    int c;

    assert(f != NULL);
    assert(end != NULL);

    c = escm_input_getc(f);
    while (c != EOF && !strchr(end, c)) {
        if (c == '\\') {
            c = escm_input_getc(f);
            switch (c) {
            case 'a': c = '\a'; break;
            case 'b': c = '\b'; break;
            case 'f': c = '\f'; break;
            case 'n': c = '\n'; break;
            case 'r': c = '\r'; break;
            case 't': c = '\t'; break;
            case 'v': c = '\v'; break;
            case '\\': case '"': break;
            default:
                strbuf[len++] = '\\';
                break; /* keep the new character */
            }
            strbuf[len++] = c;
        } else
            strbuf[len++] = c;
        c = escm_input_getc(f);
    }

    if (!f->end)
        escm_input_ungetc(f, c);
    strbuf[len] = '\0';

    return xstrdup(strbuf);
}

/**
 * @brief get a string. Each character is passed to "fun" which must return 1
 * if the character is valid, 0 else (cf ctype.h)
 */
char *
escm_input_getstr_fun(escm_input *f, int (*fun)(int), int casesens)
{
    size_t len = 0;
    int c;

    assert(f != NULL);

    do {
        c = escm_input_getc(f);
        if (!casesens)
            c = tolower(c);
        strbuf[len++] = c;
    } while (c != EOF && fun(c));

    if (!f->end)
        escm_input_ungetc(f, c);
    strbuf[len - 1] = '\0';

    return xstrdup(strbuf);
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
        escm_printf(outp, "%s:", f->d.file.name);
        if (f->d.file.line == -1)
            escm_putc(outp, ':');
        else
            escm_printf(outp, "%ld:", f->d.file.line);
        if (f->d.file.car == -1)
            escm_printf(outp, ": ");
        else
            escm_printf(outp, "%ld: ", f->d.file.car);
    } else {
#ifdef ESCM_USE_UNICODE
        escm_printf(outp, "\"%ls\":%d: ", f->d.str.str,
                    f->d.str.cur - f->d.str.str);
#else
        escm_printf(outp, "\"%s\":%d: ", f->d.str.str,
                    f->d.str.cur - f->d.str.str);
#endif
    }
}

/**
 * @brief return the next char in the stream
 */
int
escm_input_getc(escm_input *f)
{
    int c;

    assert(f != NULL);

    if (f->end)
        return EOF;

    if (f->type == INPUT_FILE) {
        if (f->d.file.un > 0)
            c = f->d.file.ub[--f->d.file.un];
        else
            c = fgetc(f->d.file.fp);

        if (c == EOF)
            f->end = 1;
        else if (c == '\n') {
            if (f->d.file.line != -1)
                f->d.file.line++;
            f->d.file.car = 0;
        } else {
            if (f->d.file.car != -1)
                f->d.file.car++;
        }
    } else {
        if (*f->d.str.cur == '\0')
            f->end = 1, c = EOF;
        else
            c = *f->d.str.cur++;
    }

    return c;
}

int
escm_input_peek(escm_input *f)
{
    int c;

    assert(f != NULL);

    if (f->end)
        return EOF;

    if (f->type == INPUT_FILE) {
        if (f->d.file.un > 0)
            return f->d.file.ub[f->d.file.un - 1];

        c = fgetc(f->d.file.fp);

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
escm_input_ungetc(escm_input *f, int c)
{
    assert(f != NULL);

    if (f->type == INPUT_FILE) {
        if (f->d.file.car != -1) {
            if (f->d.file.car > 0)
                f->d.file.car--;
            else if (f->d.file.car == 0 && c == '\n' && f->d.file.line != -1)
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
