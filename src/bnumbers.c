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
#include <limits.h>
#ifdef ESCM_USE_MATH
# include <math.h>
#endif

#include "escheme.h"
#include "bnumbers.h"

static unsigned long bnumbertype = 0;

static void number_print(escm *, escm_bnumber *, escm_output *, int);
static int number_equal(escm *, escm_bnumber *, escm_bnumber *, int);
static int number_parsetest(escm *, int);
static escm_atom *number_parse(escm *);

static escm_bnumber *inputtonumber(escm *, escm_input *, int);
static long pgcd(long, long);
#ifdef ESCM_USE_STRINGS
static char *bintostr(long);
#endif
static inline int isnumber(int);

void
escm_bnumbers_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) free;
    t->d.c.fprint = (Escm_Fun_Print) number_print;
    t->d.c.fequal = (Escm_Fun_Equal) number_equal;
    t->d.c.fparsetest = number_parsetest;
    t->d.c.fparse = number_parse;

    bnumbertype = escm_type_add(e, t);

    if (!escm_type_ison(ESCM_TYPE_BOOL)) {
	escm_bnumber *n;

	n = xmalloc(sizeof *n);
	n->fixnum = 1, n->d.ival = 0;
	e->FALSE = escm_atom_new(e, bnumbertype, n);

	n = xmalloc(sizeof *n);
	n->fixnum = 1, n->d.ival = 1;
	e->TRUE = escm_atom_new(e, bnumbertype, n);
    }

    (void) escm_procedure_new(e, "number?", 1, 1, escm_bnumber_p, NULL);
    (void) escm_procedure_new(e, "integer?", 1, 1, escm_binteger_p, NULL);
    (void) escm_procedure_new(e, "real?", 1, 1, escm_breal_p, NULL);

    (void) escm_procedure_new(e, "=", 2, -1, escm_beq, NULL);
    (void) escm_procedure_new(e, "<", 2, -1, escm_blt, NULL);
    (void) escm_procedure_new(e, ">", 2, -1, escm_bgt, NULL);
    (void) escm_procedure_new(e, "<=", 2, -1, escm_ble, NULL);
    (void) escm_procedure_new(e, ">=", 2, -1, escm_bge, NULL);

    (void) escm_procedure_new(e, "+", 0, -1, escm_badd, NULL);
    (void) escm_procedure_new(e, "-", 1, -1, escm_bsub, NULL);
    (void) escm_procedure_new(e, "*", 0, -1, escm_bmul, NULL);
    (void) escm_procedure_new(e, "/", 1, -1, escm_bdiv, NULL);

    (void) escm_procedure_new(e, "quotient", 2, 2, escm_bquotient, NULL);
    (void) escm_procedure_new(e, "remainder", 2, 2, escm_bremainder, NULL);
    (void) escm_procedure_new(e, "modulo", 2, 2, escm_bmodulo, NULL);

    (void) escm_procedure_new(e, "gcd", 0, -1, escm_bgcd, NULL);
    (void) escm_procedure_new(e, "lcm", 0, -1, escm_blcm, NULL);

    (void) escm_procedure_new(e, "numerator", 1, 1, escm_bnumerator, NULL);
    (void) escm_procedure_new(e, "denominator", 1, 1, escm_bdenominator, NULL);

#ifdef ESCM_USE_MATH
    (void) escm_procedure_new(e, "floor", 1, 1, escm_bfloor, NULL);
    (void) escm_procedure_new(e, "ceiling", 1, 1, escm_bceiling, NULL);
    (void) escm_procedure_new(e, "truncate", 1, 1, escm_btruncate, NULL);
    (void) escm_procedure_new(e, "round", 1, 1, escm_bround, NULL);

    (void) escm_procedure_new(e, "exp", 1, 1, escm_bexp, NULL);
    (void) escm_procedure_new(e, "log", 1, 1, escm_blog, NULL);
    (void) escm_procedure_new(e, "sin", 1, 1, escm_bsin, NULL);
    (void) escm_procedure_new(e, "cos", 1, 1, escm_bcos, NULL);
    (void) escm_procedure_new(e, "tan", 1, 1, escm_btan, NULL);
    (void) escm_procedure_new(e, "asin", 1, 1, escm_basin, NULL);
    (void) escm_procedure_new(e, "acos", 1, 1, escm_bacos, NULL);
    (void) escm_procedure_new(e, "atan", 1, 2, escm_batan, NULL);

    (void) escm_procedure_new(e, "sqrt", 1, 1, escm_bsqrt, NULL);
    (void) escm_procedure_new(e, "expt", 2, 2, escm_bexpt, NULL);
#endif

#ifdef ESCM_USE_STRINGS
    (void) escm_procedure_new(e, "number->string", 1, 2, escm_bnumber_to_string,
			      NULL);
    (void) escm_procedure_new(e, "string->number", 1, 2, escm_string_to_bnumber,
			      NULL);
#endif

}

size_t
escm_bnumber_tget(void)
{
    return bnumbertype;
}

escm_atom *
escm_bint_make(escm *e, long i)
{
    escm_bnumber *n;

    n = xmalloc(sizeof *n);
    n->fixnum = 1, n->d.ival = i;

    return escm_atom_new(e, bnumbertype, n);
}

escm_atom *
escm_breal_make(escm *e, double r)
{
    escm_bnumber *n;

    n = xmalloc(sizeof *n);
    n->fixnum = 0, n->d.rval = r;

    return escm_atom_new(e, bnumbertype, n);
}

escm_atom *
escm_bnumber_p(escm *e, escm_atom *args)
{
    return (ESCM_ISNUMBER(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_binteger_p(escm *e, escm_atom *args)
{
    return ESCM_ISBINT(escm_cons_car(args)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_breal_p(escm *e, escm_atom *args)
{
    return (ESCM_ISNUMBER(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_beq(escm *e, escm_atom *args)
{
    escm_atom *a, *b;

    a = escm_cons_pop(e, &args);

    escm_assert(ESCM_ISNUMBER(a), a, e);

    for (b = escm_cons_pop(e, &args); b; b = escm_cons_pop(e, &args)) {
	escm_assert(ESCM_ISNUMBER(b), b, e);

	if (0 == escm_atom_equal(e, a, b, 0))
	    return e->FALSE;
    }

    return e->TRUE;
}

escm_atom *
escm_blt(escm *e, escm_atom *args)
{
    escm_bnumber *a, *b;
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = (escm_bnumber *) c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
	escm_assert(ESCM_ISNUMBER(c), c, e);
	b = (escm_bnumber *) c->ptr;

	if (a->fixnum) {
	    if (b->fixnum) {
		if (!(a->d.ival < b->d.ival))
		    return e->FALSE;
	    } else
		if (!DBL_LT(a->d.ival, b->d.rval))
		    return e->FALSE;
	} else {
	    if (b->fixnum) {
		if (!DBL_LT(a->d.rval, b->d.ival))
		    return e->FALSE;
	    } else
		if (!DBL_LT(a->d.rval, b->d.rval))
		    return e->FALSE;
	}

	a = b;
    }

    return e->TRUE;
}

escm_atom *
escm_bgt(escm *e, escm_atom *args)
{
    escm_bnumber *a, *b;
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = (escm_bnumber *) c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
	escm_assert(ESCM_ISNUMBER(c), c, e);
	b = (escm_bnumber *) c->ptr;

	if (a->fixnum) {
	    if (b->fixnum) {
		if (!(a->d.ival > b->d.ival))
		    return e->FALSE;
	    } else
		if (!DBL_GT(a->d.ival, b->d.rval))
		    return e->FALSE;
	} else {
	    if (b->fixnum) {
		if (!DBL_GT(a->d.rval, b->d.ival))
		    return e->FALSE;
	    } else
		if (!DBL_GT(a->d.rval, b->d.rval))
		    return e->FALSE;
	}

	a = b;
    }

    return e->TRUE;
}

escm_atom *
escm_ble(escm *e, escm_atom *args)
{
    escm_bnumber *a, *b;
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = (escm_bnumber *) c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
	escm_assert(ESCM_ISNUMBER(c), c, e);
	b = (escm_bnumber *) c->ptr;

	if (a->fixnum) {
	    if (b->fixnum) {
		if (!(a->d.ival <= b->d.ival))
		    return e->FALSE;
	    } else
		if (!DBL_LE(a->d.ival, b->d.rval))
		    return e->FALSE;
	} else {
	    if (b->fixnum) {
		if (!DBL_LE(a->d.rval, b->d.ival))
		    return e->FALSE;
	    } else
		if (!DBL_LE(a->d.rval, b->d.rval))
		    return e->FALSE;
	}

	a = b;
    }

    return e->TRUE;
}

escm_atom *
escm_bge(escm *e, escm_atom *args)
{
    escm_bnumber *a, *b;
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = (escm_bnumber *) c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
	escm_assert(ESCM_ISNUMBER(c), c, e);
	b = (escm_bnumber *) c->ptr;

	if (a->fixnum) {
	    if (b->fixnum) {
		if (!(a->d.ival >= b->d.ival))
		    return e->FALSE;
	    } else
		if (!DBL_GE(a->d.ival, b->d.rval))
		    return e->FALSE;
	} else {
	    if (b->fixnum) {
		if (!DBL_GE(a->d.rval, b->d.ival))
		    return e->FALSE;
	    } else
		if (!DBL_GE(a->d.rval, b->d.rval))
		    return e->FALSE;
	}

	a = b;
    }

    return e->TRUE;
}

escm_atom *
escm_badd(escm *e, escm_atom *params)
{
    escm_bnumber *a, *b;
    escm_atom *c;

    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 0;

    c = escm_cons_pop(e, &params);
    while (c) {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
	b = ((escm_bnumber *) c->ptr);

	if (a->fixnum) {
	    if (b->fixnum)
		a->d.ival += b->d.ival;
	    else {
		long tmp;

		tmp = a->d.ival;
		a->d.rval = (double) tmp;
		a->d.rval += b->d.rval;
		a->fixnum = 0;
	    }
	} else {
	    if (b->fixnum)
		a->d.rval += b->d.ival;
	    else
		a->d.rval += b->d.rval;
	}

	c = escm_cons_pop(e, &params);
    }

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_bsub(escm *e, escm_atom *params)
{
    escm_bnumber *a, *b;
    escm_atom *c;

    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 0;

    c = escm_cons_pop(e, &params);

    if (params != e->NIL) {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));

	memcpy(a, c->ptr, sizeof *a);

	c = escm_cons_pop(e, &params);
    }

    do {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
	b = ((escm_bnumber *) c->ptr);

	if (a->fixnum) {
	    if (b->fixnum)
		a->d.ival -= b->d.ival;
	    else {
		a->d.rval = ((double) a->d.ival) - b->d.rval;
		a->fixnum = 0;
	    }
	} else {
	    if (b->fixnum)
		a->d.rval -= b->d.ival;
	    else
		a->d.rval -= b->d.rval;
	}

	c = escm_cons_pop(e, &params);
    } while (c);

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_bmul(escm *e, escm_atom *params)
{
    escm_bnumber *a, *b;
    escm_atom *c;

    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 1;

    c = escm_cons_pop(e, &params);
    while (c) {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
	b = ((escm_bnumber *) c->ptr);

	if ((b->fixnum && b->d.ival == 0) ||
	    (!b->fixnum && DBL_EQ(b->d.ival, 0.))) {
	    a->fixnum = b->fixnum;
	    memcpy(&a->d, &b->d, sizeof b->d);
	    break;
	}

	if (a->fixnum) {
	    if (b->fixnum)
		a->d.ival *= b->d.ival;
	    else {
		long tmp;

		tmp = a->d.ival;
		a->d.rval = (double) tmp;
		a->d.rval *= b->d.rval;
		a->fixnum = 0;
	    }
	} else {
	    if (b->fixnum)
		a->d.rval *= b->d.ival;
	    else
		a->d.rval *= b->d.rval;
	}

	c = escm_cons_pop(e, &params);
    }

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_bdiv(escm *e, escm_atom *params)
{
    escm_bnumber *a, *b;
    escm_atom *c;

    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 1;

    c = escm_cons_pop(e, &params);

    if (params != e->NIL) {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));

	memcpy(a, c->ptr, sizeof *a);

	c = escm_cons_pop(e, &params);
    }

    do {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
	b = ((escm_bnumber *) c->ptr);

	if ((b->fixnum) ? b->d.ival == 0 : DBL_EQ(b->d.rval, 0)) {
	    escm_error(e, "~s: division by zero.~%", escm_fun(e));
	    escm_abort(e);
	}

	if (a->fixnum) {
	    if (b->fixnum && (a->d.ival % b->d.ival == 0))
		a->d.ival /= b->d.ival;
	    else {
		long tmp;

		tmp = a->d.ival;
		a->d.rval = (double) tmp;
		if (b->fixnum)
		    a->d.rval /= b->d.ival;
		else
		    a->d.rval /= b->d.rval;
		a->fixnum = 0;
	    }
	} else {
	    if (b->fixnum)
		a->d.rval /= b->d.ival;
	    else
		a->d.rval /= b->d.rval;
	}

	c = escm_cons_pop(e, &params);
    } while (c);

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_bquotient(escm *e, escm_atom *args)
{
    escm_atom *n, *m;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(n), n, e);

    m = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(m), m, e);
    if (escm_number_ival(m) == 0) {
        escm_error(e, "~s: undefined with 0.~%", escm_fun(e));
        escm_abort(e);
    }

    return escm_bint_make(e, escm_number_ival(n) / escm_number_ival(m));
}

escm_atom *
escm_bremainder(escm *e, escm_atom *args)
{
    escm_atom *n, *m;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(n), n, e);

    m = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(m), m, e);
    if (escm_number_ival(m) == 0) {
        escm_error(e, "~s: undefined with 0.~%", escm_fun(e));
        escm_abort(e);
    }

    return escm_bint_make(e, escm_number_ival(n) % escm_number_ival(m));
}

escm_atom *
escm_bmodulo(escm *e, escm_atom *args)
{
    escm_atom *n, *m;
    long res;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(n), n, e);

    m = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(m), m, e);
    if (escm_number_ival(m) == 0) {
        escm_error(e, "~s: undefined with 0.~%", escm_fun(e));
        escm_abort(e);
    }

    res = escm_number_ival(n) % escm_number_ival(m);
    if (res * escm_number_ival(m) < 0)
        res += escm_number_ival(m);
    return escm_bint_make(e, res);
}

escm_atom *
escm_bgcd(escm *e, escm_atom *args)
{
    escm_atom *n1, *n2;
    long a, b;

    n1 = escm_cons_pop(e, &args);
    if (!n1)
        return escm_int_make(e, 0);
    escm_assert(ESCM_ISINT(n1), n1, e);
    a = escm_number_ival(n1);

    n2 = escm_cons_pop(e, &args);
    if (!n2)
	return escm_bint_make(e, escm_number_ival(n1));
    escm_assert(ESCM_ISINT(n2), n2, e);
    b = escm_number_ival(n2);

    for (;;) {
        if (b == 0)
	    return escm_bint_make(e, a);
        if (a == 0)
	    return escm_bint_make(e, b);

        a = pgcd(a, b);

        n2 = escm_cons_pop(e, &args);
        if (!n2)
	    return escm_bint_make(e, a);

        escm_assert(ESCM_ISINT(n2), n2, e);
        b = escm_number_ival(n2);
    }
}

escm_atom *
escm_blcm(escm *e, escm_atom *args)
{
    escm_atom *n1, *n2;
    long a, b, c;

    n1 = escm_cons_pop(e, &args);
    if (!n1)
	return escm_int_make(e, 1);

    escm_assert(ESCM_ISINT(n1), n1, e);
    a = ABS(escm_number_ival(n1));

    n2 = escm_cons_pop(e, &args);
    if (!n2)
        return escm_bint_make(e, escm_number_ival(n1));

    escm_assert(ESCM_ISINT(n2), n2, e);
    b = ABS(escm_number_ival(n2));

    for (;;) {
        c = pgcd(a, b);

	if ((LONG_MAX / b) < a) {
	    escm_error(e, "~s: integer overflow.~%", escm_fun(e));
            escm_abort(e);
        }
        a = a*b / c;

        n2 = escm_cons_pop(e, &args);
        if (!n2)
	    return escm_bint_make(e, a);

        escm_assert(ESCM_ISINT(n2), n2, e);
        b = ABS(escm_number_ival(n2));
    }
}

escm_atom *
escm_bnumerator(escm *e, escm_atom *args)
{
    escm_atom *n;
    double a;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(n), n, e);

    if (ESCM_ISINT(n))
	return n;

    a = escm_number_rval(n);
    while (!DBL_EQ(a, floor(a))) {
	if (DBL_LT((LONG_MAX / 2), a)) {	
	    escm_error(e, "~s: integer overflow.~%", escm_fun(e));
	    escm_abort(e);
	}
	a *= 2;
    }

    return escm_int_make(e, (long) a);
}

escm_atom *
escm_bdenominator(escm *e, escm_atom *args)
{
    escm_atom *n;
    double a;
    long b;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(n), n, e);

    if (ESCM_ISINT(n))
	return n;

    b = 1;
    a = escm_number_rval(n);
    while (!DBL_EQ(a, floor(a))) {
	if (DBL_LT((LONG_MAX / 2), a) || DBL_LT((LONG_MAX / 2), b)) {
	    escm_error(e, "~s: integer overflow.~%", escm_fun(e));
	    escm_abort(e);
	}
	a *= 2, b *= 2;
    }

    return escm_int_make(e, b);
}

#ifdef ESCM_USE_MATH
escm_atom *
escm_bfloor(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);
    if (ESCM_ISINT(a))
	return a;

    return escm_real_make(e, floor(escm_number_rval(a)));
}

escm_atom *
escm_bceiling(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);
    if (ESCM_ISINT(a))
	return a;

    return escm_real_make(e, ceil(escm_number_rval(a)));
}

escm_atom *
escm_btruncate(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);
    if (ESCM_ISINT(a))
	return a;

# ifdef ESCM_USE_C99
    return escm_real_make(e, trunc(escm_number_rval(a)));
# else
    if (DBL_GE(escm_number_rval(a), 0.))
	return escm_real_make(e, floor(escm_number_rval(a)));
    else
	return escm_real_make(e, ceil(escm_number_rval(a)));
# endif
}

escm_atom *
escm_bround(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);
    if (ESCM_ISINT(a))
	return a;

    return escm_real_make(e, xround(escm_number_rval(a)));
}

escm_atom *
escm_bexp(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);
    a = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
	escm_number_rval(atom);

    return escm_real_make(e, exp(a));
}

escm_atom *
escm_blog(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);
    a = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
	escm_number_rval(atom);

    return escm_real_make(e, log(a));
}

escm_atom *
escm_bsin(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);
    a = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
	escm_number_rval(atom);

    return escm_real_make(e, sin(a));
}

escm_atom *
escm_bcos(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);
    a = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
	escm_number_rval(atom);

    return escm_real_make(e, cos(a));
}

escm_atom *
escm_btan(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);
    a = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
	escm_number_rval(atom);

    return escm_real_make(e, tan(a));
}

escm_atom *
escm_basin(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);
    a = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
	escm_number_rval(atom);

    return escm_real_make(e, asin(a));
}

escm_atom *
escm_bacos(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);
    a = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
	escm_number_rval(atom);

    return escm_real_make(e, acos(a));
}

escm_atom *
escm_batan(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a, b;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);
    a = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
	escm_number_rval(atom);

    atom = escm_cons_pop(e, &args);
    if (atom) {
	escm_assert(ESCM_ISNUMBER(atom), atom, e);
	b = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
	    escm_number_rval(atom);

	return escm_real_make(e, atan2(a, b));
    }
    return escm_real_make(e, atan(a));
}

escm_atom *
escm_bsqrt(escm *e, escm_atom *args)
{
    escm_atom *n;
    double a;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(n), n, e);
    a = (ESCM_ISINT(n)) ? (double) escm_number_ival(n) : escm_number_rval(n);

    a = sqrt(a);

    if (DBL_EQ(a, floor(a))) /* exact */
	return escm_int_make(e, (long) a);
    else
	return escm_real_make(e, a);
}

escm_atom *
escm_bexpt(escm *e, escm_atom *args)
{
    escm_atom *n;
    double a, b;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(n), n, e);
    a = (ESCM_ISINT(n)) ? (double) escm_number_ival(n) : escm_number_rval(n);

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(n), n, e);
    b = (ESCM_ISINT(n)) ? (double) escm_number_ival(n) : escm_number_rval(n);

    a = pow(a, b);

    if (DBL_EQ(a, floor(a))) /* exact */
	return escm_int_make(e, (long) a);
    else
	return escm_real_make(e, a);
}
#endif

#ifdef ESCM_USE_STRINGS
escm_atom *
escm_bnumber_to_string(escm *e, escm_atom *args)
{
    escm_atom *a, *b;
    char *str;
    int radix;
    int len, maxlen;

    if (!escm_type_ison(ESCM_TYPE_STRING)) {
	escm_error(e, "~s: string type is off.~%", escm_fun(e));
	escm_abort(e);
    }

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);

    radix = 10;

    b = escm_cons_pop(e, &args);
    if (b) { /* radix */
	escm_assert(ESCM_ISINT(b), b, e);
	radix = (int) escm_number_ival(b);
	if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
	    escm_error(e, "~s: radix must be either 2, 8, 10 or 16.~%",
		       escm_fun(e));
	    escm_abort(e);
	}
    }

    if (ESCM_ISINT(a)) {
	if (radix == 2) {
	    str = bintostr(escm_number_ival(a));
	    len = strlen(str);
	    maxlen = len + 1;
	} else {

	    maxlen = 22;
	    str = xmalloc(sizeof *str * maxlen);

	    switch (radix) {
	    case 8:
		len = snprintf(str, maxlen, "%lo", escm_number_ival(a));
		break;
	    case 16:
		len = snprintf(str, maxlen, "%lx", escm_number_ival(a));
		break;
	    default:
		len = snprintf(str, maxlen, "%ld", escm_number_ival(a));
		break;
	    }
	}
    } else {
	maxlen = 30;
	str = xmalloc(sizeof *str * maxlen);
	len = snprintf(str, maxlen, "%.15g", escm_number_rval(a));
    }


    if (len >= maxlen) { /* output truncated */
	escm_warning(e, "~s: the output was been truncated. The "
		     "read/write invariance may not be respected.~%",
		     escm_fun(e));
	len = maxlen - 1;
    }

#ifdef ESCM_USE_UNICODE
    a = escm_ustring_make2(e, str);
#else
    a = escm_astring_make(e, str, len);
#endif
    free(str);
    return a;
}

escm_atom *
escm_string_to_bnumber(escm *e, escm_atom *args)
{
    escm_atom *a, *b;
    escm_input *input;
    escm_bnumber *number;
    int radix;

    if (!escm_type_ison(ESCM_TYPE_STRING)) {
	escm_error(e, "~s: string type is off.~%", escm_fun(e));
	escm_abort(e);
    }

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISSTR(a), a, e);

    radix = 10;

    b = escm_cons_pop(e, &args);
    if (b) { /* radix */
	escm_assert(ESCM_ISINT(b), b, e);
	radix = (int) escm_number_ival(b);
	if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
	    escm_error(e, "~s: radix must be either 2, 8, 10 or 16.~%",
		       escm_fun(e));
	    escm_abort(e);
	}
    }

#ifdef ESCM_USE_UNICODE
    if (escm_type_ison(ESCM_TYPE_ASTRING)) {
	wchar_t *w;

	w = strtowcs(escm_astr_val(a));
	input = escm_input_str(w);
	free(w);
    } else
	input = escm_input_str(escm_ustr_val(a));
#else
    input = escm_input_str(escm_str_val(a));
#endif

    number = inputtonumber(e, input, radix);
    if (!number)
	goto err;
    if (input->end == 0) {
	free(number);
	goto err;
    }

    escm_input_close(input);
    return escm_atom_new(e, bnumbertype, number);

err:
    escm_input_close(input);
    return e->FALSE;
}
#endif

static void
number_print(escm *e, escm_bnumber *number, escm_output *stream, int lvl)
{
    (void) e;
    (void) lvl;

    if (number->fixnum)
	escm_printf(stream, "%ld", number->d.ival);
    else {
	escm_printf(stream, "%.15g", number->d.rval);
	if (DBL_EQ(number->d.rval, floor(number->d.rval)))
	    escm_putc(stream, '.');
    }
}

static int
number_equal(escm *e, escm_bnumber *n1, escm_bnumber *n2, int lvl)
{
    (void) e;
    (void) lvl;

    if (n1 == n2)
	return 1;

    if (n1->fixnum == n2->fixnum)
	return (n1->fixnum == 0) ? DBL_EQ(n1->d.rval, n2->d.rval) :
	    n1->d.ival == n2->d.ival;
    else
	return 0;
}

static int
number_parsetest(escm *e, int c)
{
    (void) e;

    if (c == '+' || c == '-') {
	int c2;
	int ret;

	c2 = escm_input_getc(e->input);
	ret = isdigit(c2) || c2 == '.';
	escm_input_ungetc(e->input, c2);

	return ret;
    } else if (isdigit(c) || c == '.')
	return 1;
    else if (c == '#') {
	int ret;

	c = escm_input_getc(e->input);
	ret = (c == 'b' || c == 'o' || c == 'x' || c == 'd');
	escm_input_ungetc(e->input, c);

	return ret;
    } else
	return 0;
}    

static escm_atom *
number_parse(escm *e)
{
    escm_bnumber *n;

    n = inputtonumber(e, e->input, 10);
    if (!n)
	return NULL;
    return escm_atom_new(e, bnumbertype, n);
}

static escm_bnumber *
inputtonumber(escm *e, escm_input *input, int radix)
{
    escm_bnumber *n;
    char *str, *ec;
    int c;

    c = escm_input_getc(input);
    if (c == '#') {
	c = escm_input_getc(input);
	switch (c) {
	case 'b': radix = 2; break;
	case 'o': radix = 8; break;
	case 'd': radix = 10; break;
	case 'x': radix = 16; break;
	default:
	    escm_parse_print(input, e->errp, "unknown character #%c.\n", c);
	    return NULL;
	}
    } else if (c == EOF)
	return NULL;
    else
	escm_input_ungetc(input, c);

    n = xmalloc(sizeof *n);

    str = escm_input_getstr_fun(input, isnumber, 1);
    if (!str)
	return NULL;

    if (strchr(str, '.') != NULL) { /* real */
	n->fixnum = 0;

	n->d.rval = strtod(str, &ec);
    } else {
	n->fixnum = 1;

	n->d.ival = strtol(str, &ec, radix);
	if (*ec == '/') {
	    char *s;
	    long l;

	    s = ec + 1;
	    l = strtol(s, &ec, radix);
	    if (l != 0) {
		if (n->d.ival % l) {
		    double d;

		    d = (double) n->d.ival / (double) l;
		    n->d.rval = d;
		    n->fixnum = 0;
		} else
		    n->d.ival /= l;
	    }
	}
    }
    if (*ec != '\0') {
	if (input->type == INPUT_FILE)
	    input->d.file.car -= strlen(str) - (ec - str) - 1;
	else
#ifdef ESCM_USE_UNICODE
	    input->d.str.cur = (wchar_t *) input->d.str.str +
		((ec - str + 1) * sizeof (wchar_t));
#else
	    input->d.str.cur = (char *) input->d.str.str + (ec - str + 1);
#endif
	escm_parse_print(input, e->errp, "Character `%c' unexpected.\n", *ec);
	free(str);
	free(n);
	return NULL;
    }

    free(str);
    return n;
}

static long
pgcd(long a, long b)
{
    long c;

    if (b == 0)
	return a;
    if (b < 0)
	b = -b;
    if (a == 0)
	return b;
    if (a < 0)
	a = -a;

    do {
	c = a % b;
	a = b, b = c;
    } while (c != 0);

    return a;
}

#ifdef ESCM_USE_STRINGS
static char *
bintostr(long a)
{
    char *buf, *p;
    long off;

    buf = xmalloc((sizeof(a) * CHAR_BIT + 1) * sizeof *buf);

    p = buf;
    for (off = (long) sizeof(a) * CHAR_BIT - 1; off >= 0; off--) {
	if (p == buf) {
	    if (((a >> off) & 0x1) != 0)
		*p++ = ((a >> off) & 0x1) ? '1' : '0';
	} else
	    *p++ = (((a >> off) & 0x1)) ? '1' : '0';
    }
    *p = '\0';

    return buf;
}
#endif

static inline int
isnumber(int c)
{
    return (strchr("+-i/#.e", c) != NULL || isxdigit(c));
}
