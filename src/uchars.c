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
#include <wchar.h>
#include <wctype.h>

#include "escheme.h"
#include "uchars.h"

static unsigned long uchartype = 0;

static void uchar_print(escm *, int, escm_output *, int);
static int uchar_equal(escm *, escm_intptr, escm_intptr, int);
static int uchar_parsetest(escm *, int);
static escm_atom *uchar_parse(escm *);
static wchar_t input_getuchar(escm *, escm_input *);
static inline escm_atom *testchar(escm *, escm_atom *, int (*)(wint_t));

struct charcode {
    wchar_t *name;
    wchar_t c;
};

#define CHCODELEN 11
static struct charcode chcode[CHCODELEN] = {
    { L"newline", L'\n' },
    { L"space", L' ' },
    { L"nul", L'\0' },
    { L"alarm", L'\a' },
    { L"backspace", L'\b' },
    { L"tab", L'\t' },
    { L"vtab", L'\v' },
    { L"page", L'\f' },
    { L"return", L'\r' },
    { L"esc", L'\x1B' },
    { L"delete", L'\x7F' }
};

void
escm_uchars_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->print.fprint = (Escm_Fun_Print) uchar_print;
    t->equal.fequal = (Escm_Fun_Equal) uchar_equal;
    t->parsetest.fparsetest = uchar_parsetest;
    t->parse.fparse = uchar_parse;

    uchartype = escm_type_add(e, t);

    e->EOF_OBJ = escm_uchar_make(e, WEOF);

    (void) escm_procedure_new(e, "char?", 1, 1, escm_uchar_p, NULL);

    (void) escm_procedure_new(e, "char=?", 2, 2, escm_uchar_eq_p, NULL);
    (void) escm_procedure_new(e, "char<?", 2, 2, escm_uchar_lt_p, NULL);
    (void) escm_procedure_new(e, "char>?", 2, 2, escm_uchar_gt_p, NULL);
    (void) escm_procedure_new(e, "char<=?", 2, 2, escm_uchar_le_p, NULL);
    (void) escm_procedure_new(e, "char>=?", 2, 2, escm_uchar_ge_p, NULL);

    (void) escm_procedure_new(e, "char-ci=?", 2, 2, escm_uchar_ci_eq_p, NULL);
    (void) escm_procedure_new(e, "char-ci<?", 2, 2, escm_uchar_ci_lt_p, NULL);
    (void) escm_procedure_new(e, "char-ci>?", 2, 2, escm_uchar_ci_gt_p, NULL);
    (void) escm_procedure_new(e, "char-ci<=?", 2, 2, escm_uchar_ci_le_p, NULL);
    (void) escm_procedure_new(e, "char-ci>=?", 2, 2, escm_uchar_ci_ge_p, NULL);

    (void) escm_procedure_new(e, "char-alphabetic?", 1, 1,
                              escm_uchar_alphabetic_p, NULL);
    (void) escm_procedure_new(e, "char-numeric?", 1, 1,
                              escm_uchar_numeric_p, NULL);
    (void) escm_procedure_new(e, "char-whitespace?", 1, 1,
                              escm_uchar_whitespace_p, NULL);
    (void) escm_procedure_new(e, "char-upper-case?", 1, 1,
                              escm_uchar_upper_case_p, NULL);
    (void) escm_procedure_new(e, "char-lower-case?", 1, 1,
                              escm_uchar_lower_case_p, NULL);

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "char->integer", 1, 1,
                              escm_uchar_to_integer, NULL);
    (void) escm_procedure_new(e, "integer->char", 1, 1,
                              escm_integer_to_uchar, NULL);
#endif

    (void) escm_procedure_new(e, "char-upcase", 1, 1,
                              escm_uchar_upcase, NULL);
    (void) escm_procedure_new(e, "char-downcase", 1, 1,
                              escm_uchar_downcase, NULL);
}

size_t
escm_uchar_tget(void)
{
    return uchartype;
}

escm_atom *
escm_uchar_make(escm *e, wchar_t c)
{
    return escm_atom_new(e, uchartype, (void *) (escm_intptr) c);
}

escm_atom *
escm_uchar_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    return ESCM_ISUCHAR(a) ? e->TRUE : e->FALSE;
}

#define ID(x) x
#define charcmp(e, args, cmp, fun)                                      \
{                                                                       \
    escm_atom *c1, *c2;                                                 \
                                                                        \
    c1 = escm_cons_pop(e, &args);                                       \
    c2 = escm_cons_pop(e, &args);                                       \
    escm_assert(ESCM_ISUCHAR(c1), c1, e);                               \
    escm_assert(ESCM_ISUCHAR(c2), c2, e);                               \
                                                                        \
    return (fun(escm_uchar_val(c1)) cmp fun(escm_uchar_val(c2))) ?      \
    e->TRUE : e->FALSE;                                                 \
}

escm_atom *
escm_uchar_eq_p(escm *e, escm_atom *args)
{
    charcmp(e, args, ==, ID);
}

escm_atom *
escm_uchar_lt_p(escm *e, escm_atom *args)
{
    charcmp(e, args, <, ID);
}

escm_atom *
escm_uchar_gt_p(escm *e, escm_atom *args)
{
    charcmp(e, args, >, ID);
}

escm_atom *
escm_uchar_le_p(escm *e, escm_atom *args)
{
    charcmp(e, args, <=, ID);
}

escm_atom *
escm_uchar_ge_p(escm *e, escm_atom *args)
{
    charcmp(e, args, >=, ID);
}

escm_atom *
escm_uchar_ci_eq_p(escm *e, escm_atom *args)
{
    charcmp(e, args, ==, towlower);
}

escm_atom *
escm_uchar_ci_lt_p(escm *e, escm_atom *args)
{
    charcmp(e, args, <, towlower);
}

escm_atom *
escm_uchar_ci_gt_p(escm *e, escm_atom *args)
{
    charcmp(e, args, >, towlower);
}

escm_atom *
escm_uchar_ci_le_p(escm *e, escm_atom *args)
{
    charcmp(e, args, <=, towlower);
}

escm_atom *
escm_uchar_ci_ge_p(escm *e, escm_atom *args)
{
    charcmp(e, args, >=, towlower);
}

escm_atom *
escm_uchar_alphabetic_p(escm *e, escm_atom *args)
{
    return testchar(e, args, iswalpha);
}

escm_atom *
escm_uchar_numeric_p(escm *e, escm_atom *args)
{
    return testchar(e, args, iswdigit);
}

escm_atom *
escm_uchar_whitespace_p(escm *e, escm_atom *args)
{
    return testchar(e, args, iswspace);
}

escm_atom *
escm_uchar_upper_case_p(escm *e, escm_atom *args)
{
    return testchar(e, args, iswupper);
}

escm_atom *
escm_uchar_lower_case_p(escm *e, escm_atom *args)
{
    return testchar(e, args, iswlower);
}

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_uchar_to_integer(escm *e, escm_atom *args)
{
    escm_atom *c;

    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, "~s: number type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUCHAR(c), c, e);

    return escm_int_make(e, (long) escm_uchar_val(c));
}

escm_atom *
escm_integer_to_uchar(escm *e, escm_atom *args)
{
    escm_atom *n;

    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, "~s: number type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(n), n, e);

    if (escm_number_ival(n) < 0) {
        escm_error(e, "~s: ~s out of range  (> 0).~%", escm_fun(e), n);
        escm_abort(e);
    }

    return escm_uchar_make(e, (wint_t) escm_number_ival(n));
}
#endif

escm_atom *
escm_uchar_upcase(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUCHAR(c), c, e);

    return escm_uchar_make(e, towupper(escm_uchar_val(c)));
}

escm_atom *
escm_uchar_downcase(escm *e, escm_atom *args)
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUCHAR(c), c, e);

    return escm_uchar_make(e, towlower(escm_uchar_val(c)));
}

static void
uchar_print(escm *e, int c, escm_output *stream, int lvl)
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
            escm_printf(stream, "%ls", chcode[i].name);
            return;
        }
    }
    if (iswprint(c))
        escm_printf(stream, "%lc", c);
    else
        escm_printf(stream, "x%x", (wint_t) c);
}

static int
uchar_equal(escm *e, escm_intptr c1, escm_intptr c2, int lvl)
{
    (void) e;
    (void) lvl;

    return c1 == c2;
}

static int
uchar_parsetest(escm *e, int c)
{
    if (c == '#')
        return escm_input_peek(e->input) == '\\';

    return 0;
}

static escm_atom *
uchar_parse(escm *e)
{
    wchar_t c;

    (void) escm_input_getc(e->input), escm_input_getc(e->input); /* skip #\ */
    c = input_getuchar(e, e->input);
    if (c == L'\0' && e->err == 1)
        return NULL;

    return escm_uchar_make(e, c);
}

static wchar_t
input_getuchar(escm *e, escm_input *input)
{
    wchar_t *str;
    wchar_t c;
    size_t len;

    str = escm_input_getwstr_fun(input, iswalnum, e->casesensitive);
    len = wcslen(str);

    c = '\0';
    if (len < 1) {
        free(str);
        return escm_input_getwc(input);
    } else if (len == 1)
        c = *str;
    else {
        wchar_t *p;

        if (*str == 'x') {
            for (p = str + 1; *p != L'\0'; p++) {
                if (*p < L'0' || towlower(*p) > L'f') {
                    escm_parse_print(input, e->errp, "invalid character: "
                                     "#\\%ls.\n", str);
                    goto err;
                }
                if (*p <= L'9')
                    c <<= 4, c |= (*p - L'0');
                else
                    c <<= 4, c |= ((towlower(*p) - L'a') + 10);
            }
        } else {
            int i;

            for (i = 0; i < CHCODELEN; i++) {
                if (0 == wcscmp(chcode[i].name, str)) {
                    c = chcode[i].c;
                    break;
                }
            }
            if (i == CHCODELEN) {
                escm_parse_print(input, e->errp, "unknown character #\\%ls.", str);
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
testchar(escm *e, escm_atom *args, int (*fun)(wint_t))
{
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUCHAR(c), c, e);

    return fun(escm_uchar_val(c)) ? e->TRUE : e->FALSE;
}
