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
#include <wchar.h>
#include <wctype.h>

#include "escheme.h"

static unsigned long ustringtype = 0;

static void ustring_free(escm_ustring *);
static void ustring_print(escm *, escm_ustring *, escm_output *, int);
static int ustring_equal(escm *, escm_ustring *, escm_ustring *, int);
static int ustring_parsetest(escm *, int);
static escm_atom *ustring_parse(escm *);

void
escm_ustrings_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) ustring_free;
    t->d.c.fprint = (Escm_Fun_Print) ustring_print;
    t->d.c.fequal = (Escm_Fun_Equal) ustring_equal;
    t->d.c.fparsetest = ustring_parsetest;
    t->d.c.fparse = ustring_parse;

    ustringtype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "string?", 1, 1, escm_ustring_p, NULL);
#ifdef ESCM_USE_CHARACTERS
# ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "make-string", 1, 2, escm_make_ustring, NULL);
    (void) escm_procedure_new(e, "string-ref", 2, 2, escm_ustring_ref, NULL);
    (void) escm_procedure_new(e, "string-set!", 3, 3, escm_ustring_set_x, NULL);
# endif
    (void) escm_procedure_new(e, "string", 0, -1, escm_prim_ustring, NULL);
#endif /* USE_CHARACTERS */

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "string-length", 1, 1, escm_ustring_length,
			      NULL);
#endif

    (void) escm_procedure_new(e, "string=?", 2, 2, escm_ustring_eq_p, NULL);
    (void) escm_procedure_new(e, "string<?", 2, 2, escm_ustring_lt_p, NULL);
    (void) escm_procedure_new(e, "string>?", 2, 2, escm_ustring_gt_p, NULL);
    (void) escm_procedure_new(e, "string<=?", 2, 2, escm_ustring_le_p, NULL);
    (void) escm_procedure_new(e, "string>=?", 2, 2, escm_ustring_ge_p, NULL);

    (void) escm_procedure_new(e, "string-ci=?", 2, 2, escm_ustring_ci_eq_p,
			      NULL);
    (void) escm_procedure_new(e, "string-ci<?", 2, 2, escm_ustring_ci_lt_p,
			      NULL);
    (void) escm_procedure_new(e, "string-ci>?", 2, 2, escm_ustring_ci_gt_p,
			      NULL);
    (void) escm_procedure_new(e, "string-ci<=?", 2, 2, escm_ustring_ci_le_p,
			      NULL);
    (void) escm_procedure_new(e, "string-ci>=?", 2, 2, escm_ustring_ci_ge_p,
			      NULL);

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "substring", 3, 3, escm_subustring, NULL);
#endif
    (void) escm_procedure_new(e, "string-append", 0, -1, escm_ustring_append,
			      NULL);
    (void) escm_procedure_new(e, "string-copy", 1, 1, escm_ustring_copy, NULL);
#ifdef ESCM_USE_CHARACTERS
    (void) escm_procedure_new(e, "string-fill!", 2, 2, escm_ustring_fill_x,
			      NULL);

    (void) escm_procedure_new(e, "string->list", 1, 1, escm_ustring_to_list,
			      NULL);
    (void) escm_procedure_new(e, "list->string", 1, 1, escm_list_to_ustring,
			      NULL);
#endif
}

size_t
escm_ustring_tget(void)
{
    return ustringtype;
}

escm_atom *
escm_ustring_make(escm *e, const wchar_t *str, size_t len)
{
    escm_ustring *s;

    s = xmalloc(sizeof *s);
    s->str = xwcsdup(str), s->len = len;

    return escm_atom_new(e, ustringtype, s);
}

escm_atom *
escm_ustring_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    return ESCM_ISUSTR(a) ? e->TRUE : e->FALSE;
}

#if defined ESCM_USE_CHARACTERS && defined ESCM_USE_NUMBERS
escm_atom *
escm_make_ustring(escm *e, escm_atom *args)
{
    escm_atom *length, *c;
    wchar_t *str;
    size_t k;

    if (!escm_type_ison(ESCM_TYPE_UCHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", e->curobj);
	escm_abort(e);
    }

    length = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(length) && escm_number_ival(length) >= 0,
		length, e);
    k = (size_t) escm_number_ival(length);

    c = escm_cons_pop(e, &args);
    if (c)
	escm_assert(ESCM_ISUCHAR(c), c, e);

    str = xmalloc((k + 1) * sizeof *str);
    wmemset(str, (c != NULL) ? escm_uchar_val(c) : 0, k + 1);
    str[k] = '\0';

    c = escm_ustring_make(e, str, k);
    free(str);
    return c;
}
#endif

#ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_prim_ustring(escm *e, escm_atom *args)
{
    escm_atom *c;
    escm_cons *cons;
    wchar_t *str;
    size_t len;

    if (!escm_type_ison(ESCM_TYPE_UCHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }

    len = 0;
    for (cons = escm_cons_val(args); cons; cons = escm_cons_next(cons)) {
	escm_assert(ESCM_ISUCHAR(cons->car), cons->car, e);
	len++;
    }

    str = xmalloc((len + 1) * sizeof *str);

    len = 0;
    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args))
	str[len++] = escm_uchar_val(c);
    str[len] = L'\0';

    c = escm_ustring_make(e, str, len);
    free(str);
    return c;
}
#endif

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_ustring_length(escm *e, escm_atom *args)
{
    escm_atom *str;

    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(str), str, e);

    return escm_int_make(e, (long) escm_ustr_len(str));
}
#endif

#if defined ESCM_USE_CHARACTERS && defined ESCM_USE_NUMBERS
escm_atom *
escm_ustring_ref(escm *e, escm_atom *args)
{
    escm_atom *str, *k;
    long i;

    if (!escm_type_ison(ESCM_TYPE_UCHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(str), str, e);

    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k), k, e);
    i = escm_number_ival(k);
    escm_assert(i >= 0, k, e);

    if ((size_t) i >= escm_ustr_len(str)) {
	fprintf(stderr, "index %ld is out of range.\n", i);
	escm_abort(e);
    }

    return escm_uchar_make(e, escm_ustr_val(str)[i]);
}

escm_atom *
escm_ustring_set_x(escm *e, escm_atom *args)
{
    escm_atom *str, *k, *c;
    long i;

    if (!escm_type_ison(ESCM_TYPE_UCHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(str), str, e);

    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k), k, e);
    i = escm_number_ival(k);
    escm_assert(i >= 0, k, e);

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUCHAR(c), c, e);

    if ((size_t) i >= escm_ustr_len(str)) {
	fprintf(stderr, "index %ld is out of range.\n", i);
	escm_abort(e);
    }

    if (str->ro == 1) {
	escm_error(e, "~s: Can't modify ~s: immutable string.~%", e->curobj,
		   str);
	escm_abort(e);
    }

    escm_ustr_val(str)[i] = escm_uchar_val(c);
    return NULL;
}
#endif

escm_atom *
escm_ustring_eq_p(escm *e, escm_atom *args)
{
    escm_atom *s1, *s2;

    s1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(s1), s1, e);
    s2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(s2), s2, e);

    return (ustring_equal(e, s1->ptr, s2->ptr, 2)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_ustring_lt_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = wcscmp(escm_ustr_val(a1), escm_ustr_val(a2));
    return (i < 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_ustring_gt_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = wcscmp(escm_ustr_val(a1), escm_ustr_val(a2));
    return (i > 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_ustring_le_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = wcscmp(escm_ustr_val(a1), escm_ustr_val(a2));
    return (i <= 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_ustring_ge_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = wcscmp(escm_ustr_val(a1), escm_ustr_val(a2));
    return (i >= 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_ustring_ci_eq_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    wchar_t *s1, *s2;
    size_t i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a2), a2, e);

    if (a1 == a2 || escm_ustr_len(a1) != escm_ustr_len(a2))
	return e->TRUE;

    s1 = escm_ustr_val(a1), s2 = escm_ustr_val(a2);
    for (i = 0; i < escm_ustr_len(a1); i++) {
	if (towlower(s1[i]) != towlower(s2[i]))
	    return e->FALSE;
    }

    return e->TRUE;
}

escm_atom *
escm_ustring_ci_lt_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = xwcscasecmp(escm_ustr_val(a1), escm_ustr_val(a2));
    return (i < 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_ustring_ci_gt_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = xwcscasecmp(escm_ustr_val(a1), escm_ustr_val(a2));
    return (i > 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_ustring_ci_le_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = xwcscasecmp(escm_ustr_val(a1), escm_ustr_val(a2));
    return (i <= 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_ustring_ci_ge_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = xwcscasecmp(escm_ustr_val(a1), escm_ustr_val(a2));
    return (i >= 0) ? e->TRUE : e->FALSE;
}

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_subustring(escm *e, escm_atom *args)
{
    long start, end;
    escm_atom *str, *a;
    wchar_t *s;

    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(str), str, e);

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(a), a, e);
    start = escm_number_ival(a);
    if (start < 0 || (size_t) start > escm_ustr_len(str)) {
	fprintf(stderr, "index %ld out of range.\n", start);
	escm_abort(e);
    }

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(a), a, e);
    end = escm_number_ival(a);
    if (end < 0 || (size_t) end > escm_ustr_len(str) || end < start) {
	fprintf(stderr, "index %ld out of range.\n", end);
	escm_abort(e);
    }

    s = xmalloc((size_t) (end - start + 1) * sizeof *s);
    memcpy(s, &(escm_ustr_val(str)[start]), (size_t) (end - start) * sizeof *s);
    s[end - start] = '\0';

    a = escm_ustring_make(e, s, (size_t) end - start);
    free(s);
    return a;
}
#endif

escm_atom *
escm_ustring_append(escm *e, escm_atom *args)
{
    size_t len;
    escm_cons *c;
    escm_atom *ret;
    wchar_t *s;

    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
	escm_assert(ESCM_ISUSTR(c->car), c->car, e);
	len += escm_ustr_len(c->car);
    }

    s = xmalloc((len + 1) * sizeof *s);
    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
	memcpy(&(s[len]), escm_ustr_val(c->car),
	       escm_ustr_len(c->car) * sizeof *s);
	len += escm_ustr_len(c->car);
    }
    s[len] = L'\0';

    ret = escm_ustring_make(e, s, len);
    free(s);
    return ret;
}

escm_atom *
escm_ustring_copy(escm *e, escm_atom *args)
{
    escm_atom *str;

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(str), str, e);

    return escm_ustring_make(e, escm_ustr_val(str), escm_ustr_len(str));
}

#ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_ustring_fill_x(escm *e, escm_atom *args)
{
    escm_atom *str, *c;

    if (!escm_type_ison(ESCM_TYPE_UCHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(str), str, e);
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUCHAR(c), c, e);

    if (str->ro == 1) {
	escm_error(e, "~s: Can't modify ~s: immutable string.~%", e->curobj,
		   str);
	escm_abort(e);
    }

    wmemset(escm_ustr_val(str), escm_uchar_val(c), escm_ustr_len(str));
    return NULL;
}

escm_atom *
escm_ustring_to_list(escm *e, escm_atom *args)
{
    escm_atom *str;
    size_t i;

    if (!escm_type_ison(ESCM_TYPE_UCHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISUSTR(str), str, e);

    escm_ctx_enter(e);
    for (i = 0; i < escm_ustr_len(str); i++)
	escm_ctx_put(e, escm_uchar_make(e, escm_ustr_val(str)[i]));

    return escm_ctx_leave(e);
}

escm_atom *
escm_list_to_ustring(escm *e, escm_atom *args)
{
    escm_atom *list;

    if (!escm_type_ison(ESCM_TYPE_UCHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }

    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    return escm_prim_ustring(e, list);
}
#endif

static void
ustring_free(escm_ustring *ustring)
{
    assert(ustring != NULL);

    free(ustring->str);
    free(ustring);
}

static void
ustring_print(escm *e, escm_ustring *ustring, escm_output *stream, int lvl)
{
    (void) e;

    if (lvl == 0) {
	escm_putc(stream, '"');
	escm_print_wslashify(stream, ustring->str);
	escm_putc(stream, '"');
	return;
    }

    escm_printf(stream, "%ls", ustring->str);
}

static int
ustring_equal(escm *e, escm_ustring *s1, escm_ustring *s2, int lvl)
{
    (void) e;

    switch (lvl) {
    case 0: case 1: /* eq? & eqv?: true if same pointer */
	return s1 == s2;
    case 2: default: /* equal? */
	return (s1->len == s2->len && 0 == wcscmp(s1->str, s2->str));
    }
}

static int
ustring_parsetest(escm *e, int c)
{
    (void) e;

    return c == '"';
}

static escm_atom *
ustring_parse(escm *e)
{
    escm_atom *ret;
    wchar_t *str;

    (void) escm_input_getc(e->input); /* skip '"' */
    str = escm_input_getwtext(e->input, L"\"");
    (void) escm_input_getc(e->input); /* skip '"' */

    ret = escm_ustring_make(e, str, wcslen(str));
    free(str);
    return ret;
}
