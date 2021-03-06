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
#include <ctype.h>

#include "vectors.h"
#include "base.h"
#include "numbers.h"

static size_t vectortype = 0;

static void vector_free(escm_vector *);
static void vector_mark(escm *, escm_vector *);
static void vector_print(escm *, escm_vector *, escm_output *, int);
static int vector_equal(escm *, escm_vector *, escm_vector *, int);
static int vector_parsetest(escm *, escm_input *, tint);
static escm_atom *vector_parse(escm *, escm_input *);

void
escm_vectors_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) vector_mark;
    t->ffree = (Escm_Fun_Free) vector_free;
    t->print.fprint = (Escm_Fun_Print) vector_print;
    t->equal.fequal = (Escm_Fun_Equal) vector_equal;
    t->parsetest.fparsetest = vector_parsetest;
    t->parse.fparse = vector_parse;

    vectortype = escm_type_add(e, t);

    (void) escm_procedure_new(e, T("vector?"), 1, 1, escm_vector_p, NULL);

    (void) escm_procedure_new(e, T("vector"), 0, -1, escm_prim_vector, NULL);
#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, T("make-vector"), 1, 2, escm_make_vector, NULL);
    (void) escm_procedure_new(e, T("vector-length"), 1, 1, escm_vector_length,
                              NULL);
    (void) escm_procedure_new(e, T("vector-ref"), 2, 2, escm_vector_ref, NULL);
    (void) escm_procedure_new(e, T("vector-set!"), 3, 3, escm_vector_set_x, NULL);
#endif
    (void) escm_procedure_new(e, T("vector-fill!"), 2, 2, escm_vector_fill_x,
                              NULL);

    (void) escm_procedure_new(e, T("vector->list"), 1, 1, escm_vector_to_list,
                              NULL);
    (void) escm_procedure_new(e, T("list->vector"), 1, 1, escm_list_to_vector,
                              NULL);
}

size_t
escm_vector_tget(void)
{
    return vectortype;
}

escm_atom *
escm_vector_make(escm *e, escm_atom **vec, size_t len)
{
    escm_vector *v;

    v = xmalloc(sizeof *v);
    v->vec = vec, v->len = len;

    return escm_atom_new(e, ESCM_TYPE_VECTOR, v);
}

escm_atom *
escm_vector_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return ESCM_ISVECTOR(escm_cons_car(args)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_prim_vector(escm *e, escm_atom *args, void *nil)
{
    escm_cons *c;
    escm_atom **vec;
    size_t len;

    (void) nil;
    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c))
        len++;

    vec = xmalloc(len * sizeof *vec);
    len = 0;
    for (c = escm_cons_val(args); c; c = escm_cons_next(c))
        vec[len++] = c->car;

    return escm_vector_make(e, vec, len);
}

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_make_vector(escm *e, escm_atom *args, void *nil)
{
    escm_atom *k, *fill;
    escm_atom **vec;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k) && escm_number_ival(k) >= 0, k, e);
    fill = escm_cons_pop(e, &args);

    vec = xcalloc((size_t) escm_number_ival(k), sizeof *vec);
    if (fill) {
        size_t i;

        for (i = 0; i < (size_t) escm_number_ival(k); i++)
            vec[i] = fill;
    } /* XXX: else fill with e->FALSE? */

    return escm_vector_make(e, vec, (size_t) escm_number_ival(k));
}

escm_atom *
escm_vector_length(escm *e, escm_atom *args, void *nil)
{
    escm_atom *v;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    v = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISVECTOR(v), v, e);

    return escm_int_make(e, (long) escm_vector_len(v));
}

escm_atom *
escm_vector_ref(escm *e, escm_atom *args, void *nil)
{
    escm_atom *v, *k;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    v = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISVECTOR(v), v, e);
    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k) && escm_number_ival(k) >= 0, k, e);

    if ((size_t) escm_number_ival(k) >= escm_vector_len(v)) {
        escm_error(e, _(T("~s: index ~s out of range.~%")), escm_fun(e), k);
        escm_abort(e);
    }

    return escm_vector_val(v)->vec[escm_number_ival(k)];
}

escm_atom *
escm_vector_set_x(escm *e, escm_atom *args, void *nil)
{
    escm_atom *v, *k;

    (void) nil;
    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
        escm_error(e, _(T("~s: number type is off.~%")), escm_fun(e));
        escm_abort(e);
    }

    v = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISVECTOR(v), v, e);
    k = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(k) && escm_number_ival(k) >= 0, k, e);

    if ((size_t) escm_number_ival(k) >= escm_vector_len(v)) {
        escm_error(e, _(T("~s: index ~s out of range.~%")), escm_fun(e), k);
        escm_abort(e);
    }

    if (v->ro == 1) {
        escm_error(e, _(T("~s: Can't modify an immutable vector.~%")), escm_fun(e));
        escm_abort(e);
    }

    escm_vector_val(v)->vec[escm_number_ival(k)] = escm_cons_pop(e, &args);
    return NULL;
}
#endif

escm_atom *
escm_vector_fill_x(escm *e, escm_atom *args, void *nil)
{
    escm_atom *v, *fill;
    size_t i;

    (void) nil;
    v = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISVECTOR(v), v, e);
    fill = escm_cons_pop(e, &args);

    if (v->ro == 1) {
        escm_error(e, _(T("~s: Can't modify an immutable vector.~%")), escm_fun(e));
        escm_abort(e);
    }

    for (i = 0; i < escm_vector_len(v); i++)
        escm_vector_val(v)->vec[i] = fill;
    return NULL;
}

escm_atom *
escm_list_to_vector(escm *e, escm_atom *args, void *nil)
{
    escm_atom *list;

    (void) nil;
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    return escm_prim_vector(e, list, NULL);
}

escm_atom *
escm_vector_to_list(escm *e, escm_atom *args, void *nil)
{
    escm_atom *v;
    size_t i;
    v = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISVECTOR(v), v, e);

    (void) nil;
    escm_ctx_enter(e);
    for (i = 0; i < escm_vector_len(v); i++)
        escm_ctx_put(e, escm_vector_val(v)->vec[i]);

    return escm_ctx_leave(e);

}

static void
vector_free(escm_vector *v)
{
    free(v->vec);
    free(v);
}

static void
vector_mark(escm *e, escm_vector *vector)
{
    size_t i;

    for (i = 0; i < vector->len; i++)
        escm_atom_mark(e, vector->vec[i]);
}

static void
vector_print(escm *e, escm_vector *vector, escm_output *stream, int lvl)
{
    size_t i;

    (void) lvl;

    escm_printf(stream, T("#("));
    for (i = 0; i < vector->len; i++) {
        escm_atom_print3(e, vector->vec[i], stream);
        if (i < vector->len - 1)
            escm_putc(stream, T(' '));
    }
    escm_putc(stream, T(')'));
}

static int
vector_equal(escm *e, escm_vector *v1, escm_vector *v2, int lvl)
{
    size_t i;

    switch (lvl) {
    case 0:
    case 1:
        /* eqv? && eq?: true if same pointer */
        return v1 == v2;
    case 2:
    default:
        /* equal?: recursively compare the contents of the vector */
        if (v1->len != v2->len)
            return 0;
        for (i = 0; i < v1->len; i++) {
            if (!escm_atom_equal(e, v1->vec[i], v2->vec[i], 2))
                return 0;
        }
        return 1;
    }
}

static int
vector_parsetest(escm *e, escm_input *stream, tint c)
{
    if (c != T('#'))
        return 0;

    c = escm_input_peek(stream);
    return (c == T('(') || (e->brackets == 1 && c == T('[')));
}

static escm_atom *
vector_parse(escm *e, escm_input *stream)
{
    escm_atom *atom;
    escm_atom **vec;
    size_t len, i;
    int c;

    assert(e != NULL);

    (void) escm_input_getc(stream), escm_input_getc(stream); /* skip #( */

    escm_ctx_enter(e);

    len = 0;
    for (;;) {
        do {
            c = escm_input_getc(stream);
            if (c == ')' || (e->brackets == 1 && c == ']'))
                goto end;
            else if (!isspace(c))
                escm_input_ungetc(stream, c);
        } while (isspace(c));

        atom = escm_parse(e, stream);
        if (e->ctx->dotted) {
            escm_error(e, _(T("parse error: dotted notation is forbidden in a "))
                       "vector context.~%");
            escm_ctx_discard(e);
            return NULL;
        }
        if (e->err != 0 || atom == e->EOF_OBJ) {
            escm_ctx_discard(e);
            return NULL;
        }
        if (atom) {
            escm_ctx_put(e, atom);
            len++;
        }
    }

end:
    atom = escm_ctx_leave(e);

    vec = xcalloc(len, sizeof *vec);
    i = 0;
    while (i < len)
        vec[i++] = escm_cons_pop(e, &atom);

    return escm_vector_make(e, vec, len);
}
