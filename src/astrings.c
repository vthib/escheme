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

static unsigned long astringtype = 0;

static void astring_free(escm_astring *);
static void astring_print(escm *, escm_astring *, escm_output *, int);
static int astring_equal(escm *, escm_astring *, escm_astring *, int);
static int astring_parsetest(escm *, escm_input *, int);
static escm_atom *astring_parse(escm *, escm_input *);

void
escm_astrings_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) astring_free;
    t->print.fprint = (Escm_Fun_Print) astring_print;
    t->equal.fequal = (Escm_Fun_Equal) astring_equal;
    t->parsetest.fparsetest = astring_parsetest;
    t->parse.fparse = astring_parse;

    astringtype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "string?", 1, 1, escm_astring_p, NULL);
#ifdef ESCM_USE_CHARACTERS
# ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "make-string", 1, 2, escm_make_astring, NULL);
    (void) escm_procedure_new(e, "string-ref", 2, 2, escm_astring_ref, NULL);
    (void) escm_procedure_new(e, "string-set!", 3, 3, escm_astring_set_x, NULL);
# endif
    (void) escm_procedure_new(e, "string", 0, -1, escm_prim_astring, NULL);
#endif /* USE_CHARACTERS */

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "string-length", 1, 1, escm_astring_length,
                              NULL);
#endif

    (void) escm_procedure_new(e, "string=?", 2, 2, escm_astring_eq_p, NULL);
    (void) escm_procedure_new(e, "string<?", 2, 2, escm_astring_lt_p, NULL);
    (void) escm_procedure_new(e, "string>?", 2, 2, escm_astring_gt_p, NULL);
    (void) escm_procedure_new(e, "string<=?", 2, 2, escm_astring_le_p, NULL);
    (void) escm_procedure_new(e, "string>=?", 2, 2, escm_astring_ge_p, NULL);

    (void) escm_procedure_new(e, "string-ci=?", 2, 2, escm_astring_ci_eq_p,
                              NULL);
    (void) escm_procedure_new(e, "string-ci<?", 2, 2, escm_astring_ci_lt_p,
                              NULL);
    (void) escm_procedure_new(e, "string-ci>?", 2, 2, escm_astring_ci_gt_p,
                              NULL);
    (void) escm_procedure_new(e, "string-ci<=?", 2, 2, escm_astring_ci_le_p,
                              NULL);
    (void) escm_procedure_new(e, "string-ci>=?", 2, 2, escm_astring_ci_ge_p,
                              NULL);

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "substring", 2, 3, escm_subastring, NULL);
#endif
    (void) escm_procedure_new(e, "string-append", 0, -1, escm_astring_append,
                              NULL);
    (void) escm_procedure_new(e, "string-copy", 1, 1, escm_astring_copy, NULL);
#ifdef ESCM_USE_CHARACTERS
    (void) escm_procedure_new(e, "string-fill!", 2, 2, escm_astring_fill_x,
                              NULL);

    (void) escm_procedure_new(e, "string->list", 1, 1, escm_astring_to_list,
                              NULL);
    (void) escm_procedure_new(e, "list->string", 1, 1, escm_list_to_astring,
                              NULL);
#endif
}

size_t
escm_astring_tget(void)
{
    return astringtype;
}

escm_atom *
escm_astring_make(escm *e, const char *str, size_t len)
{
    escm_astring *s;

    s = xmalloc(sizeof *s);
    s->str = xstrdup(str), s->len = len;

    return escm_atom_new(e, astringtype, s);
}

escm_atom *
escm_astring_p(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    return ESCM_ISASTR(a) ? e->TRUE : e->FALSE;
}

#if defined ESCM_USE_CHARACTERS && defined ESCM_USE_NUMBERS
escm_atom *
escm_make_astring(escm *e, escm_atom *args, void *nil)
{
    escm_atom *length, *c;
    char *str;
    size_t k;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, "~s: character type is off.~%", escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, "~s: number type is off.~%", escm_fun(e));
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
    memset(str, (c != NULL) ? escm_achar_val(c) : 0, k);
    str[k] = '\0';

    c = escm_astring_make(e, str, k);
    free(str);
    return c;
}
#endif

#ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_prim_astring(escm *e, escm_atom *args, void *nil)
{
    escm_atom *c;
    escm_cons *cons;
    char *str;
    size_t len;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, "~s: character type is off.~%", escm_fun(e));
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
        str[len++] = escm_achar_val(c);
    str[len] = '\0';

    c = escm_astring_make(e, str, len);
    free(str);
    return c;
}
#endif

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_astring_length(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, "~s: number type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISASTR(str), str, e);

    return escm_int_make(e, (long) escm_astr_len(str));
}
#endif

#if defined ESCM_USE_CHARACTERS && defined ESCM_USE_NUMBERS
escm_atom *
escm_astring_ref(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str, *k;
    long i;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, "~s: character type is off.~%", escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, "~s: number type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISASTR(str), str, e);

    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k), k, e);
    i = escm_number_ival(k);
    escm_assert(i >= 0, k, e);

    if ((size_t) i >= escm_astr_len(str)) {
        escm_error(e, "~s: index ~s is out of range.~s", escm_fun(e), k);
        escm_abort(e);
    }

    return escm_achar_make(e, escm_astr_val(str)[i]);
}

escm_atom *
escm_astring_set_x(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str, *k, *c;
    long i;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, "~s: character type is off.~%", escm_fun(e));
        escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, "~s: number type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISASTR(str), str, e);

    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k), k, e);
    i = escm_number_ival(k);
    escm_assert(i >= 0, k, e);

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISACHAR(c), c, e);

    if ((size_t) i >= escm_astr_len(str)) {
        escm_error(e, "~s: index ~s is out of range.~s", escm_fun(e), k);
        escm_abort(e);
    }

    if (str->ro == 1) {
        escm_error(e, "~s: Can't modify ~s: immutable string.~%", escm_fun(e),
                   str);
        escm_abort(e);
    }

    escm_astr_val(str)[i] = escm_achar_val(c);
    return NULL;
}
#endif

#define cmpstr(e, args, cmp, fun)                               \
{                                                               \
    escm_atom *a1, *a2;                                         \
                                                                \
    a1 = escm_cons_pop(e, &args);                               \
    escm_assert(ESCM_ISASTR(a1), a1, e);                        \
    a2 = escm_cons_pop(e, &args);                               \
    escm_assert(ESCM_ISASTR(a2), a2, e);                        \
                                                                \
    return (fun(escm_astr_val(a1), escm_astr_val(a2)) cmp 0)    \
        ? e->TRUE : e->FALSE;                                   \
}

escm_atom *
escm_astring_eq_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, ==, strcmp);
}

escm_atom *
escm_astring_lt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, <, strcmp);
}

escm_atom *
escm_astring_gt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, >, strcmp);
}

escm_atom *
escm_astring_le_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, <=, strcmp);
}

escm_atom *
escm_astring_ge_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, >=, strcmp);
}

escm_atom *
escm_astring_ci_eq_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, ==, xstrcasecmp);
}

escm_atom *
escm_astring_ci_lt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, <, xstrcasecmp);
}

escm_atom *
escm_astring_ci_gt_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, >, xstrcasecmp);
}

escm_atom *
escm_astring_ci_le_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, <=, xstrcasecmp);
}

escm_atom *
escm_astring_ci_ge_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    cmpstr(e, args, >=, xstrcasecmp);
}

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_subastring(escm *e, escm_atom *args, void *nil)
{
    long start, end;
    escm_atom *str, *a;
    char *s;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, "~s: number type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISASTR(str), str, e);

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(a), a, e);
    start = escm_number_ival(a);
    if (start < 0 || (size_t) start > escm_astr_len(str)) {
        escm_error(e, "~s: index ~s is out of range.~s", escm_fun(e), a);
        escm_abort(e);
    }

    a = escm_cons_pop(e, &args);
    if (a) {
        escm_assert(ESCM_ISINT(a), a, e);
        end = escm_number_ival(a);
        if (end < 0 || (size_t) end > escm_astr_len(str) || end < start) {
            escm_error(e, "~s: index ~s is out of range.~s", escm_fun(e), a);
            escm_abort(e);
        }
    } else
        end = escm_astr_len(str);

    s = xmalloc((size_t) (end - start + 1) * sizeof *s);
    memcpy(s, &(escm_astr_val(str)[start]), (size_t) end - start);
    s[end - start] = '\0';

    a = escm_astring_make(e, s, (size_t) end - start);
    free(s);
    return a;
}
#endif

escm_atom *
escm_astring_append(escm *e, escm_atom *args, void *nil)
{
    size_t len;
    escm_cons *c;
    escm_atom *ret;
    char *s;

    (void) nil;
    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
        escm_assert(ESCM_ISASTR(c->car), c->car, e);
        len += escm_astr_len(c->car);
    }

    s = xmalloc((len + 1) * sizeof *s);
    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
        memcpy(&(s[len]), escm_astr_val(c->car), escm_astr_len(c->car));
        len += escm_astr_len(c->car);
    }
    s[len] = '\0';

    ret = escm_astring_make(e, s, len);
    free(s);
    return ret;
}

escm_atom *
escm_astring_copy(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str;

    (void) nil;
    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISASTR(str), str, e);

    return escm_astring_make(e, escm_astr_val(str), escm_astr_len(str));
}

#ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_astring_fill_x(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str, *c;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, "~s: character type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISASTR(str), str, e);
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    if (str->ro == 1) {
        escm_error(e, "~s: Can't modify ~s: immutable string.~%", escm_fun(e),
                   str);
        escm_abort(e);
    }

    memset(escm_astr_val(str), escm_achar_val(c), escm_astr_len(str));
    return NULL;
}

escm_atom *
escm_astring_to_list(escm *e, escm_atom *args, void *nil)
{
    escm_atom *str;
    char *p;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
        escm_error(e, "~s: character type is off.~%", escm_fun(e));
        escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISASTR(str), str, e);

    escm_ctx_enter(e);
    for (p = escm_astr_val(str); *p != '\0'; p++)
        escm_ctx_put(e, escm_achar_make(e, *p));

    return escm_ctx_leave(e);
}

escm_atom *
escm_list_to_astring(escm *e, escm_atom *args, void *nil)
{
    escm_atom *list;

    (void) nil;
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    return escm_prim_astring(e, list, NULL);
}
#endif

static void
astring_free(escm_astring *astring)
{
    assert(astring != NULL);

    free(astring->str);
    free(astring);
}

static void
astring_print(escm *e, escm_astring *astring, escm_output *stream, int lvl)
{
    (void) e;

    if (lvl == 0) {
        escm_putc(stream, '"');
        escm_print_slashify(stream, astring->str);
        escm_putc(stream, '"');
        return;
    }

    escm_printf(stream, "%s", astring->str);
}

static int
astring_equal(escm *e, escm_astring *s1, escm_astring *s2, int lvl)
{
    (void) e;

    switch (lvl) {
    case 0: case 1: /* eq? & eqv?: true if same pointer */
        return s1 == s2;
    case 2: default: /* equal? */
        return (s1->len == s2->len && 0 == strcmp(s1->str, s2->str));
    }
}

static int
astring_parsetest(escm *e, escm_input *stream, int c)
{
    (void) e;
    (void) stream;

    return c == '"';
}

static escm_atom *
astring_parse(escm *e, escm_input *stream)
{
    escm_atom *ret;
    char *str;

    (void) escm_input_getc(stream); /* skip '"' */
    str = escm_input_gettext(stream, "\"");
    (void) escm_input_getc(stream); /* skip '"' */

    ret = escm_astring_make(e, str, strlen(str));
    free(str);
    ret->ro = 1;
    return ret;
}
