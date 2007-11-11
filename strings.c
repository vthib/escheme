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

static unsigned long stringtype = 0;

static void string_free(escm_string *);
static void string_print(escm *, escm_string *, escm_output *, int);
static int string_equal(escm *, escm_string *, escm_string *, int);
static int string_parsetest(escm *, int);
static escm_atom *string_parse(escm *);

void
escm_strings_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) string_free;
    t->d.c.fprint = (Escm_Fun_Print) string_print;
    t->d.c.fequal = (Escm_Fun_Equal) string_equal;
    t->d.c.fparsetest = string_parsetest;
    t->d.c.fparse = string_parse;

    stringtype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "string?", 1, 1, escm_string_p, NULL);
#ifdef ESCM_USE_CHARACTERS
# ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "make-string", 1, 2, escm_make_string, NULL);
    (void) escm_procedure_new(e, "string-ref", 2, 2, escm_string_ref, NULL);
    (void) escm_procedure_new(e, "string-set!", 3, 3, escm_string_set_x, NULL);
# endif
    (void) escm_procedure_new(e, "string", 0, -1, escm_prim_string, NULL);
#endif /* USE_CHARACTERS */

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "string-length", 1, 1, escm_string_length,
			      NULL);
#endif

    (void) escm_procedure_new(e, "string=?", 2, 2, escm_string_eq_p, NULL);
    (void) escm_procedure_new(e, "string<?", 2, 2, escm_string_lt_p, NULL);
    (void) escm_procedure_new(e, "string>?", 2, 2, escm_string_gt_p, NULL);
    (void) escm_procedure_new(e, "string<=?", 2, 2, escm_string_le_p, NULL);
    (void) escm_procedure_new(e, "string>=?", 2, 2, escm_string_ge_p, NULL);

    (void) escm_procedure_new(e, "string-ci=?", 2, 2, escm_string_ci_eq_p,
			      NULL);
    (void) escm_procedure_new(e, "string-ci<?", 2, 2, escm_string_ci_lt_p,
			      NULL);
    (void) escm_procedure_new(e, "string-ci>?", 2, 2, escm_string_ci_gt_p,
			      NULL);
    (void) escm_procedure_new(e, "string-ci<=?", 2, 2, escm_string_ci_le_p,
			      NULL);
    (void) escm_procedure_new(e, "string-ci>=?", 2, 2, escm_string_ci_ge_p,
			      NULL);

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "substring", 3, 3, escm_substring, NULL);
#endif
    (void) escm_procedure_new(e, "string-append", 0, -1, escm_string_append,
			      NULL);
    (void) escm_procedure_new(e, "string-copy", 1, 1, escm_string_copy, NULL);
#ifdef ESCM_USE_CHARACTERS
    (void) escm_procedure_new(e, "string-fill!", 2, 2, escm_string_fill_x,
			      NULL);

    (void) escm_procedure_new(e, "string->list", 1, 1, escm_string_to_list,
			      NULL);
    (void) escm_procedure_new(e, "list->string", 1, 1, escm_list_to_string,
			      NULL);
#endif
}

size_t
escm_string_tget(void)
{
    return stringtype;
}

escm_atom *
escm_string_make(escm *e, const char *str, size_t len)
{
    escm_string *s;

    s = xmalloc(sizeof *s);
    s->str = xstrdup(str), s->len = len;

    return escm_atom_new(e, stringtype, s);
}

escm_atom *
escm_string_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    return ESCM_ISSTR(a) ? e->TRUE : e->FALSE;
}

#if defined ESCM_USE_CHARACTERS && defined ESCM_USE_NUMBERS
escm_atom *
escm_make_string(escm *e, escm_atom *args)
{
    escm_atom *length, *c;
    char *str;
    size_t k;

    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
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
	escm_assert(ESCM_ISCHAR(c), c, e);

    str = xmalloc((k + 1) * sizeof *str);
    memset(str, (c != NULL) ? escm_char_val(c) : 0, k);
    str[k] = '\0';

    c = escm_string_make(e, str, k);
    free(str);
    return c;
}
#endif

#ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_prim_string(escm *e, escm_atom *args)
{
    escm_atom *c;
    escm_cons *cons;
    char *str;
    size_t len;

    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
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
    str[len] = '\0';

    c = escm_string_make(e, str, len);
    free(str);
    return c;
}
#endif

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_string_length(escm *e, escm_atom *args)
{
    escm_atom *str;

    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    return escm_int_make(e, (long) escm_str_len(str));
}
#endif

#if defined ESCM_USE_CHARACTERS && defined ESCM_USE_NUMBERS
escm_atom *
escm_string_ref(escm *e, escm_atom *args)
{
    escm_atom *str, *k;
    long i;

    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k), k, e);
    i = escm_number_ival(k);
    escm_assert(i >= 0, k, e);

    if ((size_t) i >= escm_str_len(str)) {
	fprintf(stderr, "index %ld is out of range.\n", i);
	escm_abort(e);
    }

    return escm_char_make(e, escm_str_val(str)[i]);
}

escm_atom *
escm_string_set_x(escm *e, escm_atom *args)
{
    escm_atom *str, *k, *c;
    long i;

    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k), k, e);
    i = escm_number_ival(k);
    escm_assert(i >= 0, k, e);

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    if ((size_t) i >= escm_str_len(str)) {
	fprintf(stderr, "index %ld is out of range.\n", i);
	escm_abort(e);
    }

    if (str->ro == 1) {
	fprintf(stderr, "string-set!: Can't modify an immutable string.\n");
	escm_abort(e);
    }

    escm_str_val(str)[i] = escm_char_val(c);
    return NULL;
}
#endif

escm_atom *
escm_string_eq_p(escm *e, escm_atom *args)
{
    escm_atom *s1, *s2;

    s1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(s1), s1, e);
    s2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(s2), s2, e);

    return (string_equal(e, s1->ptr, s2->ptr, 2)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_string_lt_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = strcmp(escm_str_val(a1), escm_str_val(a2));
    return (i < 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_string_gt_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = strcmp(escm_str_val(a1), escm_str_val(a2));
    return (i > 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_string_le_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = strcmp(escm_str_val(a1), escm_str_val(a2));
    return (i <= 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_string_ge_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = strcmp(escm_str_val(a1), escm_str_val(a2));
    return (i >= 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_string_ci_eq_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    char *s1, *s2;
    size_t i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a2), a2, e);

    if (a1 == a2 || escm_str_len(a1) != escm_str_len(a2))
	return e->TRUE;

    s1 = escm_str_val(a1), s2 = escm_str_val(a2);
    for (i = 0; i < escm_str_len(a1); i++) {
	if (tolower(s1[i]) != tolower(s2[i]))
	    return e->FALSE;
    }

    return e->TRUE;
}

escm_atom *
escm_string_ci_lt_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = xstrcasecmp(escm_str_val(a1), escm_str_val(a2));
    return (i < 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_string_ci_gt_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = xstrcasecmp(escm_str_val(a1), escm_str_val(a2));
    return (i > 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_string_ci_le_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = xstrcasecmp(escm_str_val(a1), escm_str_val(a2));
    return (i <= 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_string_ci_ge_p(escm *e, escm_atom *args)
{
    escm_atom *a1, *a2;
    int i;

    a1 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a1), a1, e);
    a2 = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a2), a2, e);

    if (a1 == a2)
	return e->FALSE;

    i = xstrcasecmp(escm_str_val(a1), escm_str_val(a2));
    return (i >= 0) ? e->TRUE : e->FALSE;
}

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_substring(escm *e, escm_atom *args)
{
    long start, end;
    escm_atom *str, *a;
    char *s;

    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(a), a, e);
    start = escm_number_ival(a);
    if (start < 0 || (size_t) start > escm_str_len(str)) {
	fprintf(stderr, "index %ld out of range.\n", start);
	escm_abort(e);
    }

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(a), a, e);
    end = escm_number_ival(a);
    if (end < 0 || (size_t) end > escm_str_len(str) || end < start) {
	fprintf(stderr, "index %ld out of range.\n", end);
	escm_abort(e);
    }

    s = xmalloc((size_t) (end - start + 1) * sizeof *s);
    memcpy(s, &(escm_str_val(str)[start]), (size_t) end - start);
    s[end - start] = '\0';

    a = escm_string_make(e, s, (size_t) end - start);
    free(s);
    return a;
}
#endif

escm_atom *
escm_string_append(escm *e, escm_atom *args)
{
    size_t len;
    escm_cons *c;
    escm_atom *ret;
    char *s;

    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
	escm_assert(ESCM_ISSTR(c->car), c->car, e);
	len += escm_str_len(c->car);
    }

    s = xmalloc((len + 1) * sizeof *s);
    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c)) {
	memcpy(&(s[len]), escm_str_val(c->car), escm_str_len(c->car));
	len += escm_str_len(c->car);
    }
    s[len] = '\0';

    ret = escm_string_make(e, s, len);
    free(s);
    return ret;
}

escm_atom *
escm_string_copy(escm *e, escm_atom *args)
{
    escm_atom *str;

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    return escm_string_make(e, escm_str_val(str), escm_str_len(str));
}

#ifdef ESCM_USE_CHARACTERS
escm_atom *
escm_string_fill_x(escm *e, escm_atom *args)
{
    escm_atom *str, *c;

    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCHAR(c), c, e);

    if (str->ro == 1) {
	fprintf(stderr, "string-set!: Can't modify an immutable string.\n");
	escm_abort(e);
    }

    memset(escm_str_val(str), escm_char_val(c), escm_str_len(str));
    return NULL;
}

escm_atom *
escm_string_to_list(escm *e, escm_atom *args)
{
    escm_atom *str;
    char *p;

    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }

    str = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(str), str, e);

    escm_ctx_enter(e);
    for (p = escm_str_val(str); *p != '\0'; p++)
	escm_ctx_put(e, escm_char_make(e, *p));

    return escm_ctx_leave(e);
}

escm_atom *
escm_list_to_string(escm *e, escm_atom *args)
{
    escm_atom *list;

    if (!escm_type_ison(ESCM_TYPE_CHAR)) {
	escm_error(e, "~s: character type is off.~%", e->curobj);
	escm_abort(e);
    }

    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    return escm_prim_string(e, list);
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
	escm_putc(stream, '"');
	escm_print_slashify(stream, string->str);
	escm_putc(stream, '"');
	return;
    }

    escm_printf(stream, "%s", string->str);
#if 0
    n = mbstowcs(NULL, string->str, 0) + 1;
    if (n == 0)
	fprintf(stream, "\"%s\"", string->str);
    else {
	wchar_t *wc;
	wc = xcalloc(n, sizeof *wc);
	(void) mbstowcs(wc, string->str, n); /* check -1 ? */

	fprintf(stream, "\"%ls\"", wc);

	free(wc);
    }

    fprintf(stream, "-> %ld", (n != 0) ? n - 1 : string->len);
#endif
}

static int
string_equal(escm *e, escm_string *s1, escm_string *s2, int lvl)
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
string_parsetest(escm *e, int c)
{
    (void) e;

    return c == '"';
}    

static escm_atom *
string_parse(escm *e)
{
    escm_atom *ret;
    char *str;

    (void) escm_input_getc(e->input); /* skip '"' */
    str = escm_input_gettext(e->input, "\"");
    (void) escm_input_getc(e->input); /* skip '"' */

    ret = escm_string_make(e, str, strlen(str));
    free(str);
    return ret;
}
