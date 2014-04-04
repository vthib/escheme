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

#include "chars.h"
#include "base.h"
#include "numbers.h"

static size_t chartype = 0;

static void char_print(escm *, tint, escm_output *, int);
static int char_equal(escm *, escm_intptr, escm_intptr, int);
static int char_parsetest(escm *, escm_input *, tint);
static escm_atom *char_parse(escm *, escm_input *);
static tint input_getchar(escm *, escm_input *);
static inline escm_atom *testchar(escm *, escm_atom *, int (*)(tint));

struct charcode {
    tchar *name;
    tint c;
};

#define CHCODELEN 11
static struct charcode chcode[CHCODELEN] = {
    { T("newline"), T('\n') },
    { T("space"), T(' ') },
    { T("nul"), T('\0') },
    { T("alarm"), T('\a') },
    { T("backspace"), T('\b') },
    { T("tab"), T('\t') },
    { T("vtab"), T('\v') },
    { T("page"), T('\f') },
    { T("return"), T('\r') },
    { T("esc"), T('\x1B') },
    { T("delete"), T('\x7F') }
};

void
escm_chars_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->print.fprint = (Escm_Fun_Print) char_print;
    t->equal.fequal = (Escm_Fun_Equal) char_equal;
    t->parsetest.fparsetest = char_parsetest;
    t->parse.fparse = char_parse;

    chartype = escm_type_add(e, t);

    e->EOF_OBJ = escm_char_make(e, TEOF);

    (void) escm_procedure_new(e, T("char?"), 1, 1, escm_char_p, NULL);

    (void) escm_procedure_new(e, T("char=?"), 2, 2, escm_char_eq_p, NULL);
    (void) escm_procedure_new(e, T("char<?"), 2, 2, escm_char_lt_p, NULL);
    (void) escm_procedure_new(e, T("char>?"), 2, 2, escm_char_gt_p, NULL);
    (void) escm_procedure_new(e, T("char<=?"), 2, 2, escm_char_le_p, NULL);
    (void) escm_procedure_new(e, T("char>=?"), 2, 2, escm_char_ge_p, NULL);

    (void) escm_procedure_new(e, T("char-ci=?"), 2, 2, escm_char_ci_eq_p, NULL);
    (void) escm_procedure_new(e, T("char-ci<?"), 2, 2, escm_char_ci_lt_p, NULL);
    (void) escm_procedure_new(e, T("char-ci>?"), 2, 2, escm_char_ci_gt_p, NULL);
    (void) escm_procedure_new(e, T("char-ci<=?"), 2, 2, escm_char_ci_le_p, NULL);
    (void) escm_procedure_new(e, T("char-ci>=?"), 2, 2, escm_char_ci_ge_p, NULL);

    (void) escm_procedure_new(e, T("char-alphabetic?"), 1, 1,
                              escm_char_alphabetic_p, NULL);
    (void) escm_procedure_new(e, T("char-numeric?"), 1, 1,
                              escm_char_numeric_p, NULL);
    (void) escm_procedure_new(e, T("char-whitespace?"), 1, 1,
                              escm_char_whitespace_p, NULL);
    (void) escm_procedure_new(e, T("char-upper-case?"), 1, 1,
                              escm_char_upper_case_p, NULL);
    (void) escm_procedure_new(e, T("char-lower-case?"), 1, 1,
                              escm_char_lower_case_p, NULL);

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, T("char->integer"), 1, 1,
                              escm_char_to_integer, NULL);
    (void) escm_procedure_new(e, T("integer->char"), 1, 1,
                              escm_integer_to_char, NULL);
#endif

    (void) escm_procedure_new(e, T("char-upcase"), 1, 1,
                              escm_char_upcase, NULL);
    (void) escm_procedure_new(e, T("char-downcase"), 1, 1,
                              escm_char_downcase, NULL);
}

size_t
escm_char_tget(void)
{
    return chartype;
}

escm_atom *
escm_char_make(escm *e, tint c)
{
    if (c == TEOF)
        return e->EOF_OBJ;
    return escm_atom_new(e, chartype, (void *) (escm_intptr) c);
}

escm_atom *
escm_char_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    return ESCM_ISCHAR(a) ? e->TRUE : e->FALSE;
}

#define ID(x) x
#define charcmp(e, args, cmp, fun)                                      \
{                                                                       \
    escm_atom *c1, *c2;                                                 \
                                                                        \
    c1 = escm_cons_pop(e, &args);                                       \
    c2 = escm_cons_pop(e, &args);                                       \
    escm_assert(ESCM_ISCHAR(c1), c1, e);                               \
    escm_assert(ESCM_ISCHAR(c2), c2, e);                               \
                                                                        \
    return (fun(escm_char_val(c1)) cmp fun(escm_char_val(c2))) ?      \
    e->TRUE : e->FALSE;                                                 \
}

escm_atom *
escm_char_eq_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, ==, ID);
}

escm_atom *
escm_char_lt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, <, ID);
}

escm_atom *
escm_char_gt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, >, ID);
}

escm_atom *
escm_char_le_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, <=, ID);
}

escm_atom *
escm_char_ge_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, >=, ID);
}

escm_atom *
escm_char_ci_eq_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, ==, totlower);
}

escm_atom *
escm_char_ci_lt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, <, totlower);
}

escm_atom *
escm_char_ci_gt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, >, totlower);
}

escm_atom *
escm_char_ci_le_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, <=, totlower);
}

escm_atom *
escm_char_ci_ge_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    charcmp(e, args, >=, totlower);
}

escm_atom *
escm_char_alphabetic_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, istalpha);
}

escm_atom *
escm_char_numeric_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, istdigit);
}

escm_atom *
escm_char_whitespace_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, istspace);
}

escm_atom *
escm_char_upper_case_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, istupper);
}

escm_atom *
escm_char_lower_case_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return testchar(e, args, istlower);
}

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_char_to_integer(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return escm_int_make(e, escm_char_val(c));
}

escm_atom *
escm_integer_to_char(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(n), n, e);

    if (escm_number_ival(n) < 0) {
        escm_error(e, _(T("~s: ~s out of range  (> 0).~%")), escm_fun(e), n);
        escm_abort(e);
    }

    return escm_char_make(e, (tint) escm_number_ival(n));
}
#endif

escm_atom *
escm_char_upcase(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return escm_char_make(e, totupper(escm_char_val(c)));
}

escm_atom *
escm_char_downcase(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return escm_char_make(e, totlower(escm_char_val(c)));
}

static void
char_print(escm *e, tint c, escm_output *stream, int lvl)
{
    int i;

    (void) e;

    if (lvl == 1) {
        if (c == TEOF)
            escm_printf(stream, T("#<eof-object>"));
        escm_putc(stream, c);
        return;
    }

    if (c == TEOF) {
        escm_printf(stream, T("#<eof-object>"));
        return;
    }

    escm_printf(stream, T("#\\"));

    for (i = 0; i < CHCODELEN; i++) {
        if (c == chcode[i].c) {
            escm_printf(stream, T("%") TFMT T("s"), chcode[i].name);
            return;
        }
    }
    if (istprint(c))
        escm_printf(stream, T("%") TFMT T("c"), c);
    else
        escm_printf(stream, T("x%") TFMT T("x"), c);
}

static int
char_equal(escm *e, escm_intptr c1, escm_intptr c2, int lvl)
{
    (void) e;
    (void) lvl;

    return c1 == c2;
}

static int
char_parsetest(escm *e, escm_input *stream, tint c)
{
    (void) e;

    if (c == T('#'))
        return escm_input_peek(stream) == T('\\');

    return 0;
}

static escm_atom *
char_parse(escm *e, escm_input *stream)
{
    tint c;

    (void) escm_input_getc(stream), escm_input_getc(stream); /* skip #\ */
    c = input_getchar(e, stream);
    if (c == T('\0') && e->err == 1)
        return NULL;

    return escm_char_make(e, c);
}

static tint
input_getchar(escm *e, escm_input *input)
{
    tchar *str;
    tint c;
    size_t len;

    str = escm_input_getstr_fun(input, istalnum, e->casesensitive);
    len = tcslen(str);

    c = T('\0');
    if (len < 1) {
        free(str);
        return escm_input_getc(input);
    } else if (len == 1)
        c = *str;
    else {
        tchar *p;

        if (*str == T('x')) {
            for (p = str + 1; *p != T('\0'); p++) {
                if (*p < T('0') || totlower(*p) > T('f')) {
                    escm_parse_print(input, e->errp, _(T("invalid character:"))
                                     T(" #\\%") TFMT T("s.\n"), str);
                    goto err;
                }
                if (*p <= T('9'))
                    c <<= 4, c |= (*p - T('0'));
                else
                    c <<= 4, c |= ((tolower(*p) - T('a')) + 10);
            }
        } else {
            int i;

            for (i = 0; i < CHCODELEN; i++) {
                if (0 == tcscmp(chcode[i].name, str)) {
                    c = chcode[i].c;
                    break;
                }
            }
            if (i == CHCODELEN) {
                escm_parse_print(input, e->errp, _(T("unknown character "))
                                 T("#\\%") TFMT T("s."), str);
                goto err;
            }
        }
    }

    free(str);
    return c;

err:
    free(str);
    e->err = 1;
    return T('\0');
}

static inline escm_atom *
testchar(escm *e, escm_atom *args, int (*fun)(tint))
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    return fun(escm_char_val(c)) ? e->TRUE : e->FALSE;
}
