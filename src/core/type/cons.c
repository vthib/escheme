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
#include <assert.h>
#include <ctype.h>

#include "cons.h"
#include "utils.h"
#include "escm.h"
#include "atom.h"
#include "output.h"
#include "input.h"

static size_t constype;

static void cons_mark(escm *, escm_cons *);
static void cons_print(escm *, escm_cons *, escm_output *, int);
static int cons_equal(escm *, escm_cons *, escm_cons *, int);
static int cons_parsetest(escm *, escm_input *, tint);
static escm_atom *cons_parse(escm *, escm_input *);
static escm_atom *cons_eval(escm *, escm_cons *);

void
escm_cons_init(escm *e)
{
    escm_type *t;

	assert(e != NULL);

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) cons_mark;
    t->ffree = (Escm_Fun_Free) free;
    t->print.fprint = (Escm_Fun_Print) cons_print;
    t->equal.fequal = (Escm_Fun_Equal) cons_equal;
    t->parsetest.fparsetest = cons_parsetest;
    t->parse.fparse = cons_parse;
    t->eval.feval = (Escm_Fun_Eval) cons_eval;

    constype = escm_type_add(e, t);

    e->NIL = escm_atom_new(e, constype, NULL);
}

size_t
escm_cons_tget(void)
{
	return constype;
}

escm_atom *
escm_cons_make(escm *e, escm_atom *car, escm_atom *cdr)
{
    escm_cons *p;

    p = xmalloc(sizeof *p);
    p->car = car, p->cdr = cdr;

    return escm_atom_new(e, ESCM_TYPE_CONS, p);
}

escm_atom *
escm_cons_pop(escm *e, escm_atom **cons)
{
    escm_atom *o;

    if (!*cons || !escm_cons_val(*cons))
        return NULL;

    o = escm_cons_val(*cons)->car;
    if (ESCM_ISCONS(escm_cons_val(*cons)->cdr))
        *cons = escm_cons_val(*cons)->cdr;
    else
        *cons = e->NIL;

    return o;
}

int
escm_cons_isin(escm *e, escm_atom *cons, escm_atom *elem, int lvl)
{
    escm_atom *a;

    if (!ESCM_ISCONS(cons))
        return 0;

    for (a = escm_cons_pop(e, &cons); a; a = escm_cons_pop(e, &cons)) {
        if (escm_atom_equal(e, a, elem, lvl))
            return 1;
    }

    return 0;
}

static void
cons_mark(escm *e, escm_cons *cons)
{
    if (!cons)
        return;

    escm_atom_mark(e, cons->car);
    escm_atom_mark(e, cons->cdr);
}

static void
cons_print(escm *e, escm_cons *cons, escm_output *stream, int lvl)
{
#if ESCM_CIRCULAR_LIST == 2
    escm_cons *c, *end;
    size_t i;

    escm_putc(stream, T('('));

    e->curobj->marked = 1; /* mark all atoms to check circular lists */
    for (c = cons, end = c; c; c = escm_cons_next(c), end = c) {
        escm_atom_print4(e, c->car, stream, lvl);
        if (!ESCM_ISCONS(c->cdr)) {
            escm_printf(stream, T(" . "));
            escm_atom_print4(e, c->cdr, stream, lvl);
            break;
        } else if (c->cdr->marked == 1) {
            escm_printf(stream, T(" #"));
            break;
        } else if (c->cdr != e->NIL)
            escm_printf(stream, T(" "));
        c->cdr->marked = 1;
    }

    e->curobj->marked = 0;
    if (cons == end)
        escm_putc(stream, T('0'));
    else {
        for (i = 1, c = cons; c != end; c = escm_cons_next(c), i++) {
            if (end && ESCM_ISCONS(c->cdr) && escm_cons_val(c->cdr) == end)
                escm_printf(stream, T("%ld"), i);
            c->cdr->marked = 0;
        }
    }
#else
    (void) lvl;

    escm_putc(stream, T('('));

    for (; cons != NULL; cons = escm_cons_next(cons)) {
        escm_atom_print4(e, cons->car, stream, lvl);
        if (!ESCM_ISCONS(cons->cdr)) {
            escm_printf(stream, T(" . "));
            escm_atom_print4(e, cons->cdr, stream, lvl);
            break;
        } else if (cons->cdr != e->NIL)
            escm_putc(stream, T(' '));
    }
#endif
    escm_putc(stream, T(')'));
}

static int
cons_equal(escm *e, escm_cons *c1, escm_cons *c2, int lvl)
{
    switch (lvl) {
    default:
    case 0:
    case 1:
        /* eqv? && eq?: true if same pointer */
        return c1 == c2;
    case 2:
        /* equal?: recursively compare the contents of the cons */
        for (; c1 != NULL; c1 = escm_cons_next(c1), c2 = escm_cons_next(c2)) {
            if (!c2)
                return 0;
            if (!escm_atom_equal(e, c1->car, c2->car, 2))
                return 0;
            if (!ESCM_ISCONS(c1->cdr))
                return (ESCM_ISCONS(c2->cdr)) ? 0 :
                    escm_atom_equal(e, c1->cdr, c2->cdr, 2);
        }
        return !c2;
    }
}

static int
cons_parsetest(escm *e, escm_input *stream, tint c)
{
    (void) e;
    (void) stream;

    return (c == T('(') || (e->brackets == 1 && c == T('[')));
}

static escm_atom *
cons_parse(escm *e, escm_input *stream)
{
    escm_atom *atom;
    tint c;

    assert(e != NULL);

    (void) escm_input_getc(stream); /* skip '(' */

    escm_ctx_enter(e);

    for (;;) {
        do {
            c = escm_input_getc(stream);
            if (c == T(')') || (e->brackets == 1 && c == T(']')))
                goto end;
            else if (!isspace(c))
                escm_input_ungetc(stream, c);
        } while (isspace(c));

        atom = escm_parse(e, stream);
        if (e->err != 0 || atom == e->EOF_OBJ) {
            escm_ctx_discard(e);
            return NULL;
        }
        if (atom)
            escm_ctx_put(e, atom);
    }

end:
    atom = escm_ctx_leave(e);

    return atom;
}

static escm_atom *
cons_eval(escm *e, escm_cons *cons)
{
    escm_atom *atomfun;

    if (!cons) {
        escm_error(e, _(T("~s: object isn't applicable.~%")), e->NIL);
        escm_abort(e);
    }

    atomfun = escm_atom_eval(e, cons->car);
    if (e->err == 1)
        return NULL;
    if (!atomfun) {
        escm_error(e, _(T("~s: expression do not yield an applicable "))
                   _(T("value.~%")), cons->car);
        escm_abort(e);
    }

    return escm_atom_exec(e, atomfun, cons->cdr);
}
