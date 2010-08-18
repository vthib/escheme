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
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <ctype.h>

#include "escheme.h"
#include "strings.h"

static unsigned long stringtype = 0;

static void string_free(escm_string *);
static void string_print(escm *, escm_string *, escm_output *, int);
static int string_equal(escm *, escm_string *, escm_string *, int);
static int string_parsetest(escm *, escm_input *, tint);
static escm_atom *string_parse(escm *, escm_input *);

void
escm_strings_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) string_free;
    t->print.fprint = (Escm_Fun_Print) string_print;
    t->equal.fequal = (Escm_Fun_Equal) string_equal;
    t->parsetest.fparsetest = string_parsetest;
    t->parse.fparse = string_parse;

    stringtype = escm_type_add(e, t);

    (void) escm_procedure_new(e, T("string?"), 1, 1, escm_string_p, NULL);
#ifdef ESCM_USE_CHARACTERS
# ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, T("make-string"), 1, 2, escm_make_string,
                              NULL);
    (void) escm_procedure_new(e, T("string-ref"), 2, 2, escm_string_ref, NULL);
    (void) escm_procedure_new(e, T("string-set!"), 3, 3, escm_string_set_x,
                              NULL);
# endif
    (void) escm_procedure_new(e, T("string"), 0, -1, escm_prim_string, NULL);
#endif /* USE_CHARACTERS */

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, T("string-length"), 1, 1, escm_string_length,
                              NULL);
#endif

    (void) escm_procedure_new(e, T("string=?"), 2, 2, escm_string_eq_p, NULL);
    (void) escm_procedure_new(e, T("string<?"), 2, 2, escm_string_lt_p, NULL);
    (void) escm_procedure_new(e, T("string>?"), 2, 2, escm_string_gt_p, NULL);
    (void) escm_procedure_new(e, T("string<=?"), 2, 2, escm_string_le_p, NULL);
    (void) escm_procedure_new(e, T("string>=?"), 2, 2, escm_string_ge_p, NULL);

    (void) escm_procedure_new(e, T("string-ci=?"), 2, 2, escm_string_ci_eq_p,
                              NULL);
    (void) escm_procedure_new(e, T("string-ci<?"), 2, 2, escm_string_ci_lt_p,
                              NULL);
    (void) escm_procedure_new(e, T("string-ci>?"), 2, 2, escm_string_ci_gt_p,
                              NULL);
    (void) escm_procedure_new(e, T("string-ci<=?"), 2, 2, escm_string_ci_le_p,
                              NULL);
    (void) escm_procedure_new(e, T("string-ci>=?"), 2, 2, escm_string_ci_ge_p,
                              NULL);

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, T("substring"), 2, 3, escm_substring, NULL);
#endif
    (void) escm_procedure_new(e, T("string-append"), 0, -1, escm_string_append,
                              NULL);
    (void) escm_procedure_new(e, T("string-copy"), 1, 1, escm_string_copy,
                              NULL);
#ifdef ESCM_USE_CHARACTERS
    (void) escm_procedure_new(e, T("string-fill!"), 2, 2, escm_string_fill_x,
                              NULL);

    (void) escm_procedure_new(e, T("string->list"), 1, 1, escm_string_to_list,
                              NULL);
    (void) escm_procedure_new(e, T("list->string"), 1, 1, escm_list_to_string,
                              NULL);
#endif
}

size_t
escm_string_tget(void)
{
    return stringtype;
}

escm_atom *
escm_string_make(escm *e, const tchar *str, size_t len)
{
    escm_string *s;

    s = xmalloc(sizeof *s);
    s->str = tcsdup(str), s->len = len;

    return escm_atom_new(e, stringtype, s);
}

escm_atom *
escm_string_make2(escm *e, const tchar *str)
{
    escm_string *s;
    size_t len;

    len = tcslen(str);

    s = xmalloc(sizeof *s);
    s->str = xmalloc((len + 1) * sizeof *str);
    tmemcpy(s->str, str, len);
    s->str[len] = T('\0');
    s->len = len;

    return escm_atom_new(e, stringtype, s);
}

escm_atom *
escm_string_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    return ESCM_ISSTR(a) ? e->TRUE : e->FALSE;
}

#if defined ESCM_USE_CHARACTERS && defined ESCM_USE_NUMBERS
escm_atom *
escm_make_string(escm *e, escm_atom *args, void *nil)
{
    escm_atom *length, *c;
    escm_string *s;
    tchar *str;
    size_t k;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    length = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(length) && escm_number_ival(length) >= 0,
                length, e);
    k = (size_t) escm_number_ival(length);

    c = escm_cons_pop(e, &args);
    if (c)
        escm_assert(ESCM_ISCHAR(c), c, e);

    str = xmalloc((k + 1) * sizeof *str);
    tmemset(str, (c != NULL) ? escm_char_val(c) : T('\0'), k);
    str[k] = T('\0');

    s = xmalloc(sizeof *s);
    s->str = str, s->len = k;
    return escm_atom_new(e, stringtype, s);
}
#endif

#ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_prim_string(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;
    escm_cons *cons;
    tchar *str;
    size_t len;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    len = 0;
    for (cons = escm_cons_val(args); cons; cons = escm_cons_next(cons)) {
        escm_assert(ESCM_ISCHAR(cons->car), cons->car, e);
        len++;
    }

    str = xmalloc((len + 1) * sizeof *str);

    len = 0;
    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args))
        str[len++] = escm_char_val(c);
    str[len] = T('\0');

    c = escm_string_make(e, str, len);
    free(str);
    return c;
}
#endif

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_string_length(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    return escm_int_make(e, (long) escm_str_len(str));
}
#endif

#if defined ESCM_USE_CHARACTERS && defined ESCM_USE_NUMBERS
escm_atom *
escm_string_ref(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str, *k;
    long i;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k), k, e);
    i = escm_number_ival(k);
    escm_assert(i >= 0, k, e);

    if ((size_t) i >= escm_str_len(str)) {
        escm_error(e, _(T("~s: index ~s is out of range.~%")), escm_fun(e), k);
        escm_abort(e);
    }

    return escm_char_make(e, escm_str_val(str)[i]);
}

escm_atom *
escm_string_set_x(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str, *k, *c;
    long i;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);
    if (str->ro == 1) {
        escm_error(e, _(T("~s: Can't modify ~s: immutable string.~%")), escm_fun(e),
                   str);
        escm_abort(e);
    }


    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k), k, e);
    i = escm_number_ival(k);
    escm_assert(i >= 0, k, e);

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    if ((size_t) i >= escm_str_len(str)) {
        escm_error(e, _(T("~s: index ~s is out of range.~%")), escm_fun(e), k);
        escm_abort(e);
    }

    escm_str_val(str)[i] = escm_char_val(c);
    return NULL;
}
#endif

#define cmpstr(e, args, cmp, fun)                               \
{                                                               \
    escm_atom *a1, *a2;                                         \
                                                                \
    a1 = escm_cons_pop(e, &args);                               \
    escm_assert(ESCM_ISSTR(a1), a1, e);                        \
    a2 = escm_cons_pop(e, &args);                               \
    escm_assert(ESCM_ISSTR(a2), a2, e);                        \
                                                                \
    return (fun(escm_str_val(a1), escm_str_val(a2)) cmp 0)    \
        ? e->TRUE : e->FALSE;                                   \
}

escm_atom *
escm_string_eq_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, ==, tcscmp);
}

escm_atom *
escm_string_lt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, <, tcscmp);
}

escm_atom *
escm_string_gt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, >, tcscmp);
}

escm_atom *
escm_string_le_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, <=, tcscmp);
}

escm_atom *
escm_string_ge_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, >=, tcscmp);
}

escm_atom *
escm_string_ci_eq_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, ==, xtcscasecmp);
}

escm_atom *
escm_string_ci_lt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, <, xtcscasecmp);
}

escm_atom *
escm_string_ci_gt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, >, xtcscasecmp);
}

escm_atom *
escm_string_ci_le_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, <=, xtcscasecmp);
}

escm_atom *
escm_string_ci_ge_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, >=, xtcscasecmp);
}

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_substring(escm *e, escm_atom *args, void *nil)
{
    long start, end;
    escm_atom *str, *a;
    tchar *s;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(a), a, e);
    start = escm_number_ival(a);
    if (start < 0 || (size_t) start > escm_str_len(str)) {
        escm_error(e, _(T("~s: index ~s is out of range.~%")), escm_fun(e), a);
        escm_abort(e);
    }

    a = escm_cons_pop(e, &args);
    if (a) {
        escm_assert(ESCM_ISINT(a), a, e);
        end = escm_number_ival(a);
        if (end < 0 || (size_t) end > escm_str_len(str) || end < start) {
            escm_error(e, _(T("~s: index ~s is out of range.~%")),
                       escm_fun(e), a);
            escm_abort(e);
        }
    } else
        end = escm_str_len(str);

    s = xmalloc((size_t) (end - start + 1) * sizeof *s);
    tmemcpy(s, &(escm_str_val(str)[start]), (size_t) (end - start));
    s[end - start] = T('\0');

    a = escm_string_make(e, s, (size_t) end - start);
    free(s);
    return a;
}
#endif

escm_atom *
escm_string_append(escm *e, escm_atom *args, void *nil)
{
    size_t len;
    escm_cons *c;
    escm_atom *ret;
    tchar *s;

    (void) nil;
    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
        escm_assert(ESCM_ISSTR(c->car), c->car, e);
        len += escm_str_len(c->car);
    }

    s = xmalloc((len + 1) * sizeof *s);
    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
        tmemcpy(&(s[len]), escm_str_val(c->car), escm_str_len(c->car));
        len += escm_str_len(c->car);
    }
    s[len] = T('\0');

    ret = escm_string_make(e, s, len);
    free(s);
    return ret;
}

escm_atom *
escm_string_copy(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str;

    (void) nil;
    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    return escm_string_make(e, escm_str_val(str), escm_str_len(str));
}

#ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_string_fill_x(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str, *c;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    if (str->ro == 1) {
        escm_error(e, _(T("~s: Can't modify ~s: immutable string.~%")),
                   escm_fun(e), str);
        escm_abort(e);
    }

    tmemset(escm_str_val(str), escm_char_val(c), escm_str_len(str));

    return NULL;
}

escm_atom *
escm_string_to_list(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str;
    size_t i;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    escm_ctx_enter(e);
    for (i = 0; i < escm_str_len(str); i++)
        escm_ctx_put(e, escm_char_make(e, escm_str_val(str)[i]));

    return escm_ctx_leave(e);
}

escm_atom *
escm_list_to_string(escm *e, escm_atom *args, void *nil)
{
    escm_atom *list;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, _(T("~s: character type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    return escm_prim_string(e, list, NULL);
}
#endif

static void
string_free(escm_string *string)
{
    assert(string != NULL);

    free(string->str);
    free(string);
}

static void
string_print(escm *e, escm_string *string, escm_output *stream, int lvl)
{
    (void) e;

    if (lvl == 0) {
        escm_putc(stream, T('"'));
        escm_print_slashify(stream, string->str);
        escm_putc(stream, T('"'));
    } else
        escm_printf(stream, T("%") TFMT T("s"), string->str);
}

static int
string_equal(escm *e, escm_string *s1, escm_string *s2, int lvl)
{
    (void) e;

    switch (lvl) {
    case 0: case 1: /* eq? & eqv?: true if same pointer */
        return s1 == s2;
    case 2: default: /* equal? */
        return (s1->len == s2->len && 0 == tcscmp(s1->str, s2->str));
    }
}

static int
string_parsetest(escm *e, escm_input *stream, tint c)
{
    (void) e;
    (void) stream;

    return c == T('"');
}

static escm_atom *
string_parse(escm *e, escm_input *stream)
{
    escm_atom *ret;
    tchar *str;

    (void) escm_input_getc(stream); /* skip '"' */
    str = escm_input_gettext(stream, T("\""));
    (void) escm_input_getc(stream); /* skip '"' */

    ret = escm_string_make(e, str, tcslen(str));
    free(str);
    ret->ro = 1;
    return ret;
}
