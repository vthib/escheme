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
#include <ctype.h>

#include "escheme.h"

static unsigned long achartype = 0;

static void achar_print(escm *, int, escm_output *, int);
static int achar_equal(escm *, escm_intptr, escm_intptr, int);
static int achar_parsetest(escm *, escm_input *, int);
static escm_atom *achar_parse(escm *, escm_input *);
static int input_getchar(escm *, escm_input *);
static inline escm_atom *testchar(escm *, escm_atom *, int (*)(int));

struct charcode {
    char *name;
    int c;
};

#define CHCODELEN 11
static struct charcode chcode[CHCODELEN] = {
    { "newline", '\n' },
    { "space", ' ' },
    { "nul", '\0' },
    { "alarm", '\a' },
    { "backspace", '\b' },
    { "tab", '\t' },
    { "vtab", '\v' },
    { "page", '\f' },
    { "return", '\r' },
    { "esc", '\x1B' },
    { "delete", '\x7F' }
};

void
escm_achars_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->print.fprint = (Escm_Fun_Print) achar_print;
    t->equal.fequal = (Escm_Fun_Equal) achar_equal;
    t->parsetest.fparsetest = achar_parsetest;
    t->parse.fparse = achar_parse;

    achartype = escm_type_add(e, t);

    e->EOF_OBJ = escm_achar_make(e, EOF);

    (void) escm_procedure_new(e, "char?", 1, 1, escm_achar_p, NULL);

    (void) escm_procedure_new(e, "char=?", 2, 2, escm_achar_eq_p, NULL);
    (void) escm_procedure_new(e, "char<?", 2, 2, escm_achar_lt_p, NULL);
    (void) escm_procedure_new(e, "char>?", 2, 2, escm_achar_gt_p, NULL);
    (void) escm_procedure_new(e, "char<=?", 2, 2, escm_achar_le_p, NULL);
    (void) escm_procedure_new(e, "char>=?", 2, 2, escm_achar_ge_p, NULL);

    (void) escm_procedure_new(e, "char-ci=?", 2, 2, escm_achar_ci_eq_p, NULL);
    (void) escm_procedure_new(e, "char-ci<?", 2, 2, escm_achar_ci_lt_p, NULL);
    (void) escm_procedure_new(e, "char-ci>?", 2, 2, escm_achar_ci_gt_p, NULL);
    (void) escm_procedure_new(e, "char-ci<=?", 2, 2, escm_achar_ci_le_p, NULL);
    (void) escm_procedure_new(e, "char-ci>=?", 2, 2, escm_achar_ci_ge_p, NULL);

    (void) escm_procedure_new(e, "char-alphabetic?", 1, 1,
                              escm_achar_alphabetic_p, NULL);
    (void) escm_procedure_new(e, "char-numeric?", 1, 1,
                              escm_achar_numeric_p, NULL);
    (void) escm_procedure_new(e, "char-whitespace?", 1, 1,
                              escm_achar_whitespace_p, NULL);
    (void) escm_procedure_new(e, "char-upper-case?", 1, 1,
                              escm_achar_upper_case_p, NULL);
    (void) escm_procedure_new(e, "char-lower-case?", 1, 1,
                              escm_achar_lower_case_p, NULL);

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "char->integer", 1, 1,
                              escm_achar_to_integer, NULL);
    (void) escm_procedure_new(e, "integer->char", 1, 1,
                              escm_integer_to_achar, NULL);
#endif

    (void) escm_procedure_new(e, "char-upcase", 1, 1,
                              escm_achar_upcase, NULL);
    (void) escm_procedure_new(e, "char-downcase", 1, 1,
                              escm_achar_downcase, NULL);
}

size_t
escm_achar_tget(void)
{
    return achartype;
}

escm_atom *
escm_achar_make(escm *e, int c)
{
    return escm_atom_new(e, achartype, (void *) (escm_intptr) c);
}

escm_atom *
escm_achar_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    return ESCM_ISACHAR(a) ? e->TRUE : e->FALSE;
}

#define ID(x) x
#define charcmp(e, args, cmp, fun)                                      \
{                                                                       \
    escm_atom *c1, *c2;                                                 \
                                                                        \
    c1 = escm_cons_pop(e, &args);                                       \
    c2 = escm_cons_pop(e, &args);                                       \
    escm_assert(ESCM_ISACHAR(c1), c1, e);                               \
    escm_assert(ESCM_ISACHAR(c2), c2, e);                               \
                                                                        \
    return (fun(escm_achar_val(c1)) cmp fun(escm_achar_val(c2))) ?      \
    e->TRUE : e->FALSE;                                                 \
}

escm_atom *
escm_achar_eq_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, ==, ID);
}

escm_atom *
escm_achar_lt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args,<, ID);
}

escm_atom *
escm_achar_gt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, >, ID);
}

escm_atom *
escm_achar_le_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, <=, ID);
}

escm_atom *
escm_achar_ge_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, >=, ID);
}

escm_atom *
escm_achar_ci_eq_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, ==, tolower);
}

escm_atom *
escm_achar_ci_lt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, <, tolower);
}

escm_atom *
escm_achar_ci_gt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, >, tolower);
}

escm_atom *
escm_achar_ci_le_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, <=, tolower);
}

escm_atom *
escm_achar_ci_ge_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, >=, tolower);
}

escm_atom *
escm_achar_alphabetic_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, isalpha);
}

escm_atom *
escm_achar_numeric_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, isdigit);
}

escm_atom *
escm_achar_whitespace_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, isspace);
}

escm_atom *
escm_achar_upper_case_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, isupper);
}

escm_atom *
escm_achar_lower_case_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, islower);
}

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_achar_to_integer(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, "~s: number type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISACHAR(c), c, e);

    return escm_int_make(e, (long) escm_achar_val(c));
}

escm_atom *
escm_integer_to_achar(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, "~s: number type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(n), n, e);

    if (escm_number_ival(n) > 255 || escm_number_ival(n) < 0) {
        escm_error(e, "~s: ~s out of range [0;255].~%", escm_fun(e), n);
        escm_abort(e);
    }

    return escm_achar_make(e, (unsigned char) escm_number_ival(n));
}
#endif

escm_atom *
escm_achar_upcase(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISACHAR(c), c, e);

    return escm_achar_make(e, toupper(escm_achar_val(c)));
}

escm_atom *
escm_achar_downcase(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISACHAR(c), c, e);

    return escm_achar_make(e, tolower(escm_achar_val(c)));
}

static void
achar_print(escm *e, int c, escm_output *stream, int lvl)
{
    int i;

    (void) e;

    if (lvl == 1) {
        if (c == EOF)
            escm_printf(stream, "#<eof-object>");
        escm_putc(stream, c);
        return;
    }

    if (c == EOF) {
        escm_printf(stream, "#<eof-object>");
        return;
    }

    escm_printf(stream, "#\\");

    for (i = 0; i < CHCODELEN; i++) {
        if (c == chcode[i].c) {
            escm_printf(stream, chcode[i].name);
            return;
        }
    }
    if (isprint(c))
        escm_printf(stream, "%c", c);
    else
        escm_printf(stream, "x%x", (unsigned char) c);
}

static int
achar_equal(escm *e, escm_intptr c1, escm_intptr c2, int lvl)
{
    (void) e;
    (void) lvl;

    return c1 == c2;
}

static int
achar_parsetest(escm *e, escm_input *stream, int c)
{
    (void) e;

    if (c == '#')
        return escm_input_peek(stream) == '\\';
    return 0;
}

static escm_atom *
achar_parse(escm *e, escm_input *stream)
{
    int c;

    (void) escm_input_getc(stream), escm_input_getc(stream); /* skip #\ */
    c = input_getchar(e, stream);
    if (c == '\0' && e->err == 1)
        return NULL;

    return escm_achar_make(e, c);
}

static int
input_getchar(escm *e, escm_input *input)
{
    char *str;
    int c;
    size_t len;

    str = escm_input_getstr_fun(input, isalnum, e->casesensitive);
    len = strlen(str);

    c = '\0';
    if (len < 1) {
        free(str);
        return escm_input_getc(input);
    } else if (len == 1)
        c = *str;
    else {
        char *p;

        if (*str == 'x') {
            if (strlen(str) > 3) {
                escm_parse_print(input, e->errp, "invalid character: #\\%s.\n",
                                 str);
                goto err;
            }

            for (p = str + 1; *p != '\0'; p++) {
                if (*p < '0' || tolower(*p) > 'f') {
                    escm_parse_print(input, e->errp, "invalid character: "
                                     "#\\%s.\n", str);
                    goto err;
                }
                if (*p <= '9')
                    c <<= 4, c |= (*p - '0');
                else
                    c <<= 4, c |= ((tolower(*p) - 'a') + 10);
            }
        } else {
            int i;

            for (i = 0; i < CHCODELEN; i++) {
                if (0 == strcmp(chcode[i].name, str)) {
                    c = chcode[i].c;
                    break;
                }
            }
            if (i == CHCODELEN) {
                escm_parse_print(input, e->errp, "unknown character #\\%s.", str);
                goto err;
            }
        }
    }

    free(str);
    return c;

err:
    free(str);
    e->err = 1;
    return '\0';
}

static inline escm_atom *
testchar(escm *e, escm_atom *args, int (*fun)(int))
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISACHAR(c), c, e);

    return fun(escm_achar_val(c)) ? e->TRUE : e->FALSE;
}
