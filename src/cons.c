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
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "escheme.h"

static unsigned long constype = 0;

static void cons_mark(escm *, escm_cons *);
static void cons_print(escm *, escm_cons *, escm_output *, int);
static int cons_equal(escm *, escm_cons *, escm_cons *, int);
static int cons_parsetest(escm *, int);
static escm_atom *cons_parse(escm *);
static escm_atom *cons_eval(escm *, escm_cons *);

void
escm_cons_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->fmark = (Escm_Fun_Mark) cons_mark;
    t->ffree = (Escm_Fun_Free) free;
    t->d.c.fprint = (Escm_Fun_Print) cons_print;
    t->d.c.fequal = (Escm_Fun_Equal) cons_equal;
    t->d.c.fparsetest = cons_parsetest;
    t->d.c.fparse = cons_parse;
    t->d.c.feval = (Escm_Fun_Eval) cons_eval;

    constype = escm_type_add(e, t);

    e->NIL = escm_atom_new(e, constype, NULL);

    (void) escm_procedure_new(e, "cons", 2, 2, escm_prim_cons, NULL);
    (void) escm_procedure_new(e, "list", 0, -1, escm_list, NULL);

    (void) escm_procedure_new(e, "car", 1, 1, escm_car, NULL);
    (void) escm_procedure_new(e, "set-car!", 2, 2, escm_set_car_x, NULL);
    (void) escm_procedure_new(e, "cdr", 1, 1, escm_cdr, NULL);
    (void) escm_procedure_new(e, "set-cdr!", 2, 2, escm_set_cdr_x, NULL);

    (void) escm_procedure_new(e, "null?", 1, 1, escm_null_p, NULL);
    (void) escm_procedure_new(e, "pair?", 1, 1, escm_pair_p, NULL);
    (void) escm_procedure_new(e, "list?", 1, 1, escm_list_p, NULL);

    (void) escm_procedure_new(e, "append", 2, 2, escm_append, NULL);
    (void) escm_procedure_new(e, "reverse", 1, 1, escm_reverse, NULL);

#ifdef ESCM_USE_NUMBERS
    (void) escm_procedure_new(e, "length", 1, 1, escm_length, NULL);
    (void) escm_procedure_new(e, "list-tail", 2, 2, escm_list_tail, NULL);
    (void) escm_procedure_new(e, "list-ref", 2, 2, escm_list_ref, NULL);
#endif

    (void) escm_procedure_new(e, "memq", 2, 2, escm_memq, NULL);
    (void) escm_procedure_new(e, "memv", 2, 2, escm_memv, NULL);
    (void) escm_procedure_new(e, "member", 2, 2, escm_member, NULL);

    (void) escm_procedure_new(e, "assq", 2, 2, escm_assq, NULL);
    (void) escm_procedure_new(e, "assv", 2, 2, escm_assv, NULL);
    (void) escm_procedure_new(e, "assoc", 2, 2, escm_assoc, NULL);
}

unsigned long
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

void
escm_cons_put(escm *e, escm_atom **cons, escm_atom *atom)
{
    escm_atom *new;

    new = escm_cons_make(e, atom, (*cons) ? *cons : e->NIL);
    *cons = new;
}

escm_atom *
escm_cons_pop(escm *e, escm_atom **cons)
{
    escm_atom *o;

    if (!*cons || !escm_cons_val(*cons))
	return NULL;

    o = escm_cons_val(*cons)->car;
    if (ESCM_ISCONS(escm_cons_val(*cons)->cdr))
	*cons = (escm_cons_val(*cons)->cdr == e->NIL) ? NULL :
	    escm_cons_val(*cons)->cdr;
    else
	*cons = NULL;

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

escm_atom *
escm_prim_cons(escm *e, escm_atom *args)
{
    (void) e;

    escm_cons_val(args)->cdr = escm_cons_val(escm_cons_val(args)->cdr)->car;

    return args;
}

escm_atom *
escm_list(escm *e, escm_atom *args)
{
    (void) e;

    return args;
}

escm_atom *
escm_car(escm *e, escm_atom *args)
{
    escm_atom *o;

    o = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(o) && escm_cons_val(o) != NULL, o, e);

    return escm_cons_car(o);
}

escm_atom *
escm_set_car_x(escm *e, escm_atom *args)
{
    escm_atom *o;

    o = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(o) && escm_cons_val(o) != NULL, o, e);

    if (o->ro == 1) {
	escm_error(e, "~s: Can't modify ~s: immutable cons.~%", escm_fun(e), o);
	escm_abort(e);
    }

    escm_cons_val(o)->car = escm_cons_pop(e, &args);
    return NULL;
}

escm_atom *
escm_cdr(escm *e, escm_atom *args)
{
    escm_atom *o;

    o = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(o) && escm_cons_val(o) != NULL, o, e);

    return escm_cons_cdr(o);
}

escm_atom *
escm_set_cdr_x(escm *e, escm_atom *args)
{
    escm_atom *o;

    o = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(o) && escm_cons_val(o) != NULL, o, e);

    if (o->ro == 1) {
	escm_error(e, "~s: Can't modify ~s: immutable cons.~%", escm_fun(e), o);
	escm_abort(e);
    }

    escm_cons_val(o)->cdr = escm_cons_pop(e, &args);
    return NULL;
}

escm_atom *
escm_null_p(escm *e, escm_atom *args)
{
    return (escm_cons_val(args)->car == e->NIL) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_pair_p(escm *e, escm_atom *args)
{
    return (ESCM_ISCONS(escm_cons_val(args)->car) &&
	    escm_cons_val(args)->car != e->NIL) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_list_p(escm *e, escm_atom *args)
{
    escm_atom *arg;
    escm_cons *c, *end;
    unsigned int res;

    arg = escm_cons_pop(e, &args);

    if (!ESCM_ISCONS(arg))
	res = 0;
    else {
	res = 1;

#if ESCM_CIRCULAR_LIST >= 1
	arg->marked = 1; /* mark all atoms to check circular lists */
#endif
	for (c = escm_cons_val(arg), end = c; c; c = escm_cons_next(c),
		 end = c) {
	    if (!ESCM_ISCONS(c->cdr)
#if ESCM_CIRCULAR_LIST >= 1
		|| c->cdr->marked == 1
#endif
		) {
		res = 0;
		break;
	    }
#if ESCM_CIRCULAR_LIST >= 1
	    c->cdr->marked = 1;
#endif
	}

#if ESCM_CIRCULAR_LIST >= 1
	arg->marked = 0;
	for (c = escm_cons_val(arg); c != end; c = escm_cons_next(c))
	    c->cdr->marked = 0;
#endif
    }

    return (res == 1) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_append(escm *e, escm_atom *args)
{
    escm_atom *flist;
    escm_cons *a;

    flist = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(flist), flist, e);

    escm_ctx_enter(e);

    for (a = escm_cons_val(flist); a; a = escm_cons_next(a)) {
	if (!ESCM_ISCONS(a->cdr)) {
	    escm_atom_printerr(e, flist);
	    escm_error(e, "~s: ~s: improper list.~%", escm_fun(e), flist);
	    escm_ctx_discard(e);
	    escm_abort(e);
	}
	escm_ctx_put(e, a->car);
    }

    escm_ctx_put_splicing(e, escm_cons_pop(e, &args));
    return escm_ctx_leave(e);
}

escm_atom *
escm_reverse(escm *e, escm_atom *args)
{
    escm_atom *new, *arg;
    escm_cons *c;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

    new = NULL;
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
	if (!ESCM_ISCONS(c->cdr)) {
	    escm_atom_printerr(e, arg);
	    escm_error(e, "~s: ~s: improper list.~%", escm_fun(e), arg);
	    escm_abort(e);
	}
	new = escm_cons_make(e, c->car, (new != NULL) ? new : e->NIL);
    }

    return new;
}

#ifdef ESCM_USE_NUMBERS
escm_atom *
escm_length(escm *e, escm_atom *args)
{
    escm_atom *arg;
    escm_cons *c, *end;
    long n;

    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", escm_fun(e));
	escm_abort(e);
    }

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

    n = 0;

#if ESCM_CIRCULAR_LIST >= 1
    arg->marked = 1; /* mark all atoms to check circular lists */
#endif
    for (c = escm_cons_val(arg), end = c; c; c = escm_cons_next(c),
	     end = c) {
	if (!ESCM_ISCONS(c->cdr)
#if ESCM_CIRCULAR_LIST >= 1
	    || (c->cdr->marked == 1)
#endif
	    ) {
	    escm_error(e, "~s: Can't compute the length of a non proper "
		       "list.~%", escm_fun(e));
	    e->err = 1;
#if ESCM_CIRCULAR_LIST >= 1
	    break;
#else
	    return NULL;
#endif
	}
#if ESCM_CIRCULAR_LIST >= 1
	c->cdr->marked = 1;
#endif
	n++;
    }

#if ESCM_CIRCULAR_LIST >= 1
    arg->marked = 0;
    for (c = escm_cons_val(arg); c != end; c = escm_cons_next(c))
	c->cdr->marked = 0;

    if (end != NULL)
	return NULL;
#endif

    return escm_int_make(e, n);
}

escm_atom *
escm_list_tail(escm *e, escm_atom *args)
{
    escm_atom *list, *ka, *atom;
    long k;

    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", escm_fun(e));
	escm_abort(e);
    }

    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    ka = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(ka), ka, e);

    k = escm_number_ival(ka);
    escm_assert(k >= 0, ka, e);
    atom = list;

    for (; k > 0; k--) {
	if (atom == e->NIL || !atom) {
	    escm_error(e, "list-tail: index ~s is too large for the list ~s.~%",
		       ka, list);
	    return NULL;
	}
	if (!ESCM_ISCONS(atom)) {
	    escm_error(e, "~s: improper list.~%", list);
	    escm_abort(e);
	}
	atom = escm_cons_val(atom)->cdr;
    }

    return atom;
}

escm_atom *
escm_list_ref(escm *e, escm_atom *args)
{
    escm_atom *sublist;

    if (!escm_type_ison(ESCM_TYPE_NUMBER)) {
	escm_error(e, "~s: number type is off.~%", escm_fun(e));
	escm_abort(e);
    }

    sublist = escm_list_tail(e, args);
    if (!sublist)
	return NULL;
    if (sublist == e->NIL) {
	escm_error(e, "~s: index too large.~%", escm_fun(e));
	escm_abort(e);
    }

    return escm_cons_val(sublist)->car;
}
#endif

escm_atom *
escm_memq(escm *e, escm_atom *args)
{
    escm_atom *elem, *list;
    escm_atom *c;

    elem = escm_cons_pop(e, &args);
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    for (c = list; c != NULL && c != e->NIL;
	 c = ESCM_ISCONS(escm_cons_val(c)->cdr) ?
	     escm_cons_val(c)->cdr : NULL) {
	if (escm_atom_equal(e, escm_cons_val(c)->car, elem, 0))
	    return c;
    }

    return e->FALSE;
}

escm_atom *
escm_memv(escm *e, escm_atom *args)
{
    escm_atom *elem, *list;
    escm_atom *c;

    elem = escm_cons_pop(e, &args);
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    for (c = escm_cons_pop(e, &list); c; c = escm_cons_pop(e, &list)) {
	if (escm_atom_equal(e, c, elem, 1))
	    return c;
    }

    return e->FALSE;
}

escm_atom *
escm_member(escm *e, escm_atom *args)
{
    escm_atom *elem, *list;
    escm_atom *c;

    elem = escm_cons_pop(e, &args);
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    for (c = list; c; c = ESCM_ISCONS(escm_cons_val(c)->cdr) ?
	     escm_cons_val(c)->cdr : NULL) {
	if (escm_atom_equal(e, escm_cons_val(c)->car, elem, 2))
	    return c;
    }

    return e->FALSE;
}

escm_atom *
escm_assq(escm *e, escm_atom *args)
{
    escm_atom *elem, *list, *pair;

    elem = escm_cons_pop(e, &args);
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    for (pair = escm_cons_pop(e, &list); pair; pair = escm_cons_pop(e, &list)) {
	escm_assert(ESCM_ISCONS(pair), pair, e);
	if (escm_atom_equal(e, escm_cons_val(pair)->car, elem, 0))
	    return pair;
    }

    return e->FALSE;
}

escm_atom *
escm_assv(escm *e, escm_atom *args)
{
    escm_atom *elem, *list, *pair;

    elem = escm_cons_pop(e, &args);
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    for (pair = escm_cons_pop(e, &list); pair; pair = escm_cons_pop(e, &list)) {
	escm_assert(ESCM_ISCONS(pair), pair, e);
	if (escm_atom_equal(e, escm_cons_val(pair)->car, elem, 1))
	    return pair;
    }

    return e->FALSE;
}

escm_atom *
escm_assoc(escm *e, escm_atom *args)
{
    escm_atom *elem, *list, *pair;

    elem = escm_cons_pop(e, &args);
    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    for (pair = escm_cons_pop(e, &list); pair; pair = escm_cons_pop(e, &list)) {
	escm_assert(ESCM_ISCONS(pair), pair, e);
	if (escm_atom_equal(e, escm_cons_val(pair)->car, elem, 2))
	    return pair;
    }

    return e->FALSE;
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

    escm_putc(stream, '(');

    e->curobj->marked = 1; /* mark all atoms to check circular lists */
    for (c = cons, end = c; c; c = escm_cons_next(c), end = c) {
	escm_atom_print4(e, c->car, stream, lvl);
	if (!ESCM_ISCONS(c->cdr)) {
	    fprintf(stream, " . ");
	    escm_atom_print4(e, c->cdr, stream, lvl);
	    break;
	} else if (c->cdr->marked == 1) {
	    escm_printf(stream, " #");
	    break;
	} else if (c->cdr != e->NIL)
	    escm_printf(stream, " ");
	c->cdr->marked = 1;
    }

    e->curobj->marked = 0;
    if (cons == end)
	escm_putc(stream, '0');
    else {
	for (i = 1, c = cons; c != end; c = escm_cons_next(c), i++) {
	    if (end && ESCM_ISCONS(c->cdr) && escm_cons_val(c->cdr) == end)
		escm_printf(stream, "%ld", i);
	    c->cdr->marked = 0;
	}
    }
#else
    (void) lvl;

    escm_putc(stream, '(');

    for (; cons != NULL; cons = escm_cons_next(cons)) {
	escm_atom_print4(e, cons->car, stream, lvl);
	if (!ESCM_ISCONS(cons->cdr)) {
	    escm_printf(stream, " . ");
	    escm_atom_print4(e, cons->cdr, stream, lvl);
	    break;
	} else if (cons->cdr != e->NIL)
	    escm_putc(stream, ' ');
    }
#endif
    escm_putc(stream, ')');
}

static int
cons_equal(escm *e, escm_cons *c1, escm_cons *c2, int lvl)
{
    switch (lvl) {
    case 0:
    case 1:
	/* eqv? && eq?: true if same pointer */
	return c1 == c2;
    case 2:
    default:
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
	return 1;
    }
}

static int
cons_parsetest(escm *e, int c)
{
    (void) e;

    return (c == '(' || (e->brackets == 1 && c == '['));
}

static escm_atom *
cons_parse(escm *e)
{
    escm_atom *atom;
    int c, open;

    assert(e != NULL);

    open = escm_input_getc(e->input);

    escm_ctx_enter(e);

    for (;;) {
	do {
	    c = escm_input_getc(e->input);
	    if (e->brackets == 1 && (c == ')' || c == ']')) {
		if ((open == '(' && c == ']') || (open == '[' && c == ')')) {
		    escm_parse_print(e, e->errp,
				     "expecting a '%c' to close a '%c'.\n",
				     (open == '(') ? ')' : ']', open);
		    escm_ctx_discard(e);
		    escm_abort(e);
		}
		goto end;
	    } else if (e->brackets == 0 && c == ')')
		goto end;
	    else if (!isspace(c))
		escm_input_ungetc(e->input, c);
	} while (isspace(c));

	atom = escm_parse(e);
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
	escm_error(e, "~s: object isn't applicable.~%", e->NIL);
	escm_abort(e);
    }

    atomfun = escm_atom_eval(e, cons->car);
    if (e->err == 1)
	return NULL;
    if (!atomfun) {
	escm_error(e, "~s: expression do not yield an applicable value.~%",
		   cons->car);
	escm_abort(e);
    }

    return escm_atom_exec(e, atomfun, cons->cdr);
}
