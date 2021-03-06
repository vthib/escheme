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
#include <math.h>

#include "numbers.h"
#include "base.h"

static size_t numbertype = 0;

static void number_print(escm *, escm_number *, escm_output *, int);
static int number_equal(escm *, escm_number *, escm_number *, int);
static int number_parsetest(escm *, escm_input *, tint);
static escm_atom *number_parse(escm *, escm_input *);

static escm_number *inputtonumber(escm *, escm_input *, int);
static long pgcd(long, long);
static inline int isnumber(tint);
static inline escm_atom *exeround(escm *, escm_atom *, double (*)(double));
static inline escm_atom *exemath(escm *, escm_atom *, double (*)(double));

void
escm_numbers_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) free;
    t->print.fprint = (Escm_Fun_Print) number_print;
    t->equal.fequal = (Escm_Fun_Equal) number_equal;
    t->parsetest.fparsetest = number_parsetest;
    t->parse.fparse = number_parse;

    numbertype = escm_type_add(e, t);

	/* cons procedures but enabled only as the number type is enabled */
    (void) escm_procedure_new(e, T("length"), 1, 1, escm_length, NULL);
    (void) escm_procedure_new(e, T("list-tail"), 2, 2, escm_list_tail, NULL);
    (void) escm_procedure_new(e, T("list-ref"), 2, 2, escm_list_ref, NULL);

    (void) escm_procedure_new(e, T("number?"), 1, 1, escm_number_p, NULL);
    (void) escm_procedure_new(e, T("integer?"), 1, 1, escm_integer_p, NULL);
    (void) escm_procedure_new(e, T("real?"), 1, 1, escm_real_p, NULL);

    (void) escm_procedure_new(e, T("="), 2, -1, escm_eq, NULL);
    (void) escm_procedure_new(e, T("<"), 2, -1, escm_lt, NULL);
    (void) escm_procedure_new(e, T(">"), 2, -1, escm_gt, NULL);
    (void) escm_procedure_new(e, T("<="), 2, -1, escm_le, NULL);
    (void) escm_procedure_new(e, T(">="), 2, -1, escm_ge, NULL);

    (void) escm_procedure_new(e, T("+"), 0, -1, escm_add, NULL);
    (void) escm_procedure_new(e, T("-"), 1, -1, escm_sub, NULL);
    (void) escm_procedure_new(e, T("*"), 0, -1, escm_mul, NULL);
    (void) escm_procedure_new(e, T("/"), 1, -1, escm_div, NULL);

    (void) escm_procedure_new(e, T("quotient"), 2, 2, escm_quotient, NULL);
    (void) escm_procedure_new(e, T("remainder"), 2, 2, escm_remainder, NULL);
    (void) escm_procedure_new(e, T("modulo"), 2, 2, escm_modulo, NULL);

    (void) escm_procedure_new(e, T("gcd"), 0, -1, escm_gcd, NULL);
    (void) escm_procedure_new(e, T("lcm"), 0, -1, escm_lcm, NULL);

    (void) escm_procedure_new(e, T("numerator"), 1, 1, escm_numerator, NULL);
    (void) escm_procedure_new(e, T("denominator"), 1, 1, escm_denominator, NULL);

    (void) escm_procedure_new(e, T("rand"), 2, 2, escm_rand, NULL);

    (void) escm_procedure_new(e, T("floor"), 1, 1, escm_floor, NULL);
    (void) escm_procedure_new(e, T("ceiling"), 1, 1, escm_ceiling, NULL);
    (void) escm_procedure_new(e, T("truncate"), 1, 1, escm_truncate, NULL);
    (void) escm_procedure_new(e, T("round"), 1, 1, escm_round, NULL);

    (void) escm_procedure_new(e, T("exp"), 1, 1, escm_exp, NULL);
    (void) escm_procedure_new(e, T("log"), 1, 1, escm_log, NULL);
    (void) escm_procedure_new(e, T("sin"), 1, 1, escm_sin, NULL);
    (void) escm_procedure_new(e, T("cos"), 1, 1, escm_cos, NULL);
    (void) escm_procedure_new(e, T("tan"), 1, 1, escm_tan, NULL);
    (void) escm_procedure_new(e, T("asin"), 1, 1, escm_asin, NULL);
    (void) escm_procedure_new(e, T("acos"), 1, 1, escm_acos, NULL);
    (void) escm_procedure_new(e, T("atan"), 1, 2, escm_atan, NULL);

    (void) escm_procedure_new(e, T("sqrt"), 1, 1, escm_sqrt, NULL);
    (void) escm_procedure_new(e, T("expt"), 2, 2, escm_expt, NULL);
}

size_t
escm_number_tget(void)
{
    return numbertype;
}

escm_atom *
escm_int_make(escm *e, long i)
{
    escm_number *n;

    n = xmalloc(sizeof *n);
    n->fixnum = 1, n->d.ival = i;

    return escm_atom_new(e, numbertype, n);
}

escm_atom *
escm_real_make(escm *e, double r)
{
    escm_number *n;

    n = xmalloc(sizeof *n);
    n->fixnum = 0, n->d.rval = r;

    return escm_atom_new(e, numbertype, n);
}

escm_atom *
escm_number_parse(escm *e, escm_input *stream, int base)
{
    escm_number *n;

    n = inputtonumber(e, stream, base);
    if (!n)
        return NULL;
    return escm_atom_new(e, numbertype, n);
}

escm_atom *
escm_length(escm *e, escm_atom *args, void *nil)
{
    escm_atom *arg;
    escm_cons *c;
#if ESCM_CIRCULAR_LIST >= 1
    escm_cons *end;
#endif
	long n;

    (void) nil;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(arg), arg, e);

    n = 0;

#if ESCM_CIRCULAR_LIST >= 1
    arg->marked = 1; /* mark all atoms to check circular lists */
    for (c = escm_cons_val(arg), end = c; c; c = escm_cons_next(c),
             end = c) {
#else
    for (c = escm_cons_val(arg); c; c = escm_cons_next(c)) {
#endif
        if (!ESCM_ISCONS(c->cdr)
#if ESCM_CIRCULAR_LIST >= 1
            || (c->cdr->marked == 1)
#endif
            ) {
            escm_error(e, _(T("~s: Can't compute the length of a non proper "))
                       _(T("list.~%")), escm_fun(e));
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
escm_list_tail(escm *e, escm_atom *args, void *nil)
{
    escm_atom *list, *ka, *atom;
    long k;

    (void) nil;

    list = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISCONS(list), list, e);

    ka = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(ka), ka, e);

    k = escm_number_ival(ka);
    escm_assert(k >= 0, ka, e);
    atom = list;

    for (; k > 0; k--) {
        if (atom == e->NIL || !atom) {
            escm_error(e, _(T("~: index ~s is too large for the list ~s.~%")),
                       escm_fun(e), ka, list);
            escm_abort(e);
        }
        if (!ESCM_ISCONS(atom)) {
            escm_error(e, _(T("~s: improper list.~%")), list);
            escm_abort(e);
        }
        atom = escm_cons_val(atom)->cdr;
    }

    return atom;
}

escm_atom *
escm_list_ref(escm *e, escm_atom *args, void *nil)
{
    escm_atom *sublist;

    (void) nil;

    sublist = escm_list_tail(e, args, NULL);
    if (!sublist)
        return NULL;
    if (!ESCM_ISCONS(sublist) || sublist == e->NIL) {
        escm_error(e, _(T("~s: index too large.~%")), escm_fun(e));
        escm_abort(e);
    }

    return escm_cons_val(sublist)->car;
}

escm_atom *
escm_number_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return (ESCM_ISNUMBER(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_integer_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return ESCM_ISINT(escm_cons_car(args)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_real_p(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return (ESCM_ISNUMBER(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_eq(escm *e, escm_atom *args, void *nil)
{
    escm_number *a, *b;
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = (escm_number *) c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISNUMBER(c), c, e);
        b = (escm_number *) c->ptr;

        if (a->fixnum) {
            if (b->fixnum) {
                if (!(a->d.ival == b->d.ival))
                    return e->FALSE;
            } else
                if (!DBL_EQ(a->d.ival, b->d.rval))
                    return e->FALSE;
        } else {
            if (b->fixnum) {
                if (!DBL_EQ(a->d.rval, b->d.ival))
                    return e->FALSE;
            } else
                if (!DBL_EQ(a->d.rval, b->d.rval))
                    return e->FALSE;
        }

        a = b;
    }

    return e->TRUE;
}

escm_atom *
escm_lt(escm *e, escm_atom *args, void *nil)
{
    escm_number *a, *b;
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = (escm_number *) c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISNUMBER(c), c, e);
        b = (escm_number *) c->ptr;

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
escm_gt(escm *e, escm_atom *args, void *nil)
{
    escm_number *a, *b;
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = (escm_number *) c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISNUMBER(c), c, e);
        b = (escm_number *) c->ptr;

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
escm_le(escm *e, escm_atom *args, void *nil)
{
    escm_number *a, *b;
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = (escm_number *) c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISNUMBER(c), c, e);
        b = (escm_number *) c->ptr;

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
escm_ge(escm *e, escm_atom *args, void *nil)
{
    escm_number *a, *b;
    escm_atom *c;

    (void) nil;
    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = (escm_number *) c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISNUMBER(c), c, e);
        b = (escm_number *) c->ptr;

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
escm_add(escm *e, escm_atom *args, void *nil)
{
    escm_number *a, *b;
    escm_atom *c;

    (void) nil;
    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 0;

    c = escm_cons_pop(e, &args);
    while (c) {
        escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
        b = ((escm_number *) c->ptr);

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

        c = escm_cons_pop(e, &args);
    }

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_sub(escm *e, escm_atom *args, void *nil)
{
    escm_number *a, *b;
    escm_atom *c;

    (void) nil;
    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 0;

    c = escm_cons_pop(e, &args);

    if (args != e->NIL) {
        escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));

        memcpy(a, c->ptr, sizeof *a);

        c = escm_cons_pop(e, &args);
    }

    do {
        escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
        b = ((escm_number *) c->ptr);

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

        c = escm_cons_pop(e, &args);
    } while (c);

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_mul(escm *e, escm_atom *args, void *nil)
{
    escm_number *a, *b;
    escm_atom *c;

    (void) nil;
    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 1;

    c = escm_cons_pop(e, &args);
    while (c) {
        escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
        b = ((escm_number *) c->ptr);

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

        c = escm_cons_pop(e, &args);
    }

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_div(escm *e, escm_atom *args, void *nil)
{
    escm_number *a, *b;
    escm_atom *c;

    (void) nil;
    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 1;

    c = escm_cons_pop(e, &args);

    if (args != e->NIL) {
        escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));

        memcpy(a, c->ptr, sizeof *a);

        c = escm_cons_pop(e, &args);
    }

    do {
        escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
        b = ((escm_number *) c->ptr);

        if ((b->fixnum) ? b->d.ival == 0 : DBL_EQ(b->d.rval, 0)) {
            escm_error(e, _(T("~s: division by zero.~%")), escm_fun(e));
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

        c = escm_cons_pop(e, &args);
    } while (c);

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_quotient(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n, *m;

    (void) nil;
    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(n), n, e);

    m = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(m), m, e);
    if (escm_number_ival(m) == 0) {
        escm_error(e, _(T("~s: undefined with 0.~%")), escm_fun(e));
        escm_abort(e);
    }

    return escm_int_make(e, escm_number_ival(n) / escm_number_ival(m));
}

escm_atom *
escm_remainder(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n, *m;

    (void) nil;
    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(n), n, e);

    m = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(m), m, e);
    if (escm_number_ival(m) == 0) {
        escm_error(e, _(T("~s: undefined with 0.~%")), escm_fun(e));
        escm_abort(e);
    }

    return escm_int_make(e, escm_number_ival(n) % escm_number_ival(m));
}

escm_atom *
escm_modulo(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n, *m;
    long res;

    (void) nil;
    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(n), n, e);

    m = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(m), m, e);
    if (escm_number_ival(m) == 0) {
        escm_error(e, _(T("~s: undefined with 0.~%")), escm_fun(e));
        escm_abort(e);
    }

    res = escm_number_ival(n) % escm_number_ival(m);
    if (res * escm_number_ival(m) < 0)
        res += escm_number_ival(m);
    return escm_int_make(e, res);
}

escm_atom *
escm_gcd(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n1, *n2;
    long a, b;

    (void) nil;
    n1 = escm_cons_pop(e, &args);
    if (!n1)
        return escm_int_make(e, 0);
    escm_assert(ESCM_ISINT(n1), n1, e);
    a = escm_number_ival(n1);

    n2 = escm_cons_pop(e, &args);
    if (!n2)
        return escm_int_make(e, escm_number_ival(n1));
    escm_assert(ESCM_ISINT(n2), n2, e);
    b = escm_number_ival(n2);

    for (;;) {
        if (b == 0)
            return escm_int_make(e, a);
        if (a == 0)
            return escm_int_make(e, b);

        a = pgcd(a, b);

        n2 = escm_cons_pop(e, &args);
        if (!n2)
            return escm_int_make(e, a);

        escm_assert(ESCM_ISINT(n2), n2, e);
        b = escm_number_ival(n2);
    }
}

escm_atom *
escm_lcm(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n1, *n2;
    long a, b, c;

    (void) nil;
    n1 = escm_cons_pop(e, &args);
    if (!n1)
        return escm_int_make(e, 1);

    escm_assert(ESCM_ISINT(n1), n1, e);
    a = ABS(escm_number_ival(n1));

    n2 = escm_cons_pop(e, &args);
    if (!n2)
        return escm_int_make(e, escm_number_ival(n1));

    escm_assert(ESCM_ISINT(n2), n2, e);
    b = ABS(escm_number_ival(n2));

    for (;;) {
        c = pgcd(a, b);

        if ((LONG_MAX / b) < a) {
            escm_error(e, _(T("~s: integer overflow.~%")), escm_fun(e));
            escm_abort(e);
        }
        a = a*b / c;

        n2 = escm_cons_pop(e, &args);
        if (!n2)
            return escm_int_make(e, a);

        escm_assert(ESCM_ISINT(n2), n2, e);
        b = ABS(escm_number_ival(n2));
    }
}

escm_atom *
escm_numerator(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n;
    double a;

    (void) nil;
    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(n), n, e);

    if (ESCM_ISINT(n))
        return n;

    a = escm_number_rval(n);
    while (!DBL_EQ(a, floor(a))) {
        if (DBL_LT((LONG_MAX / 2), a)) {
            escm_error(e, _(T("~s: integer overflow.~%")), escm_fun(e));
            escm_abort(e);
        }
        a *= 2;
    }

    return escm_int_make(e, (long) a);
}

escm_atom *
escm_denominator(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n;
    double a;
    long b;

    (void) nil;
    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(n), n, e);

    if (ESCM_ISINT(n))
        return n;

    b = 1;
    a = escm_number_rval(n);
    while (!DBL_EQ(a, floor(a))) {
        if (DBL_LT((LONG_MAX / 2), a) || DBL_LT((LONG_MAX / 2), b)) {
            escm_error(e, _(T("~s: integer overflow.~%")), escm_fun(e));
            escm_abort(e);
        }
        a *= 2, b *= 2;
    }

    return escm_int_make(e, b);
}

escm_atom *
escm_rand(escm *e, escm_atom *args, void *nil)
{
    escm_atom *start, *end;

    (void) nil;
    start = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(start), start, e);
    end = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISINT(end), end, e);

    return escm_int_make(e,
            escm_number_ival(start) + (rand() % escm_number_ival(end)));
}

escm_atom *
escm_floor(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exeround(e, args, floor);
}

escm_atom *
escm_ceiling(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exeround(e, args, ceil);
}

escm_atom *
escm_truncate(escm *e, escm_atom *args, void *nil)
{
    escm_atom *a;

    (void) nil;
    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);
    if (ESCM_ISINT(a))
        return a;

    return escm_real_make(e, trunc(escm_number_rval(a)));
}

escm_atom *
escm_round(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exeround(e, args, xround);
}

escm_atom *
escm_exp(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exemath(e, args, exp);
}

escm_atom *
escm_log(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exemath(e, args, log);
}

escm_atom *
escm_sin(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exemath(e, args, sin);
}

escm_atom *
escm_cos(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exemath(e, args, cos);
}

escm_atom *
escm_tan(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exemath(e, args, tan);
}

escm_atom *
escm_asin(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exemath(e, args, asin);
}

escm_atom *
escm_acos(escm *e, escm_atom *args, void *nil)
{
    (void) nil;
    return exemath(e, args, acos);
}

escm_atom *
escm_atan(escm *e, escm_atom *args, void *nil)
{
    escm_atom *atom;
    double a, b;

    (void) nil;
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
escm_sqrt(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n;
    double a;

    (void) nil;
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
escm_expt(escm *e, escm_atom *args, void *nil)
{
    escm_atom *n;
    double a, b;

    (void) nil;
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

static void
number_print(escm *e, escm_number *number, escm_output *stream, int lvl)
{
    (void) e;
    (void) lvl;

    if (number->fixnum)
        escm_printf(stream, T("%ld"), number->d.ival);
    else {
        escm_printf(stream, T("%.15g"), number->d.rval);
        if (DBL_EQ(number->d.rval, floor(number->d.rval)))
            escm_putc(stream, T('.'));
    }
}

static int
number_equal(escm *e, escm_number *n1, escm_number *n2, int lvl)
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
number_parsetest(escm *e, escm_input *stream, tint c)
{
    (void) e;

    if (c == T('+') || c == T('-')) {
        c = escm_input_peek(stream);
        return istdigit(c) || c == T('.');
    } else if (istdigit(c) || c == T('.'))
        return 1;
    else if (c == T('#')) {
        c = escm_input_peek(stream);
        return (c == T('b') || c == T('o') || c == T('x') || c == T('d'));
    } else
        return 0;
}

static escm_atom *
number_parse(escm *e, escm_input *stream)
{
	return escm_number_parse(e, stream, 10);
}

escm_number *
inputtonumber(escm *e, escm_input *input, int radix)
{
    escm_number *n;
    tchar *str, *ec;
    tint c;

    c = escm_input_getc(input);
    if (c == T('#')) {
        c = escm_input_getc(input);
        switch (c) {
        case T('b'): radix = 2; break;
        case T('o'): radix = 8; break;
        case T('d'): radix = 10; break;
        case T('x'): radix = 16; break;
        default:
            escm_parse_print(input, e->errp, _(T("unknown character #%")) TFMT
                             T("c.\n"), c);
            return NULL;
        }
    } else if (c == TEOF)
        return NULL;
    else
        escm_input_ungetc(input, c);

    n = xmalloc(sizeof *n);

    str = escm_input_getstr_fun(input, isnumber, 1);
    if (!str)
        return NULL;

    if (tcschr(str, T('.')) != NULL) { /* real */
        n->fixnum = 0;

        n->d.rval = tcstod(str, &ec);
    } else {
        n->fixnum = 1;

        n->d.ival = tcstol(str, &ec, radix);
        if (*ec == T('/')) {
            tchar *s;
            long l;

            s = ec + 1;
            l = tcstol(s, &ec, radix);
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
    if (*ec != T('\0')) {
        if (input->type == INPUT_FILE)
            input->d.file.car -= tcslen(str) - (ec - str) - 1;
        else
            input->d.str.cur = input->d.str.str + (ec - str + 1);
        escm_parse_print(input, e->errp, _(T("Character `%")) TFMT
                         T("c' unexpected.\n"), *ec);
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

static inline int
isnumber(tint c)
{
    return (tcschr(T("+-/.e"), c) != NULL || istxdigit(c));
}

static inline escm_atom *
exeround(escm *e, escm_atom *args, double (*fun)(double))
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);
    if (ESCM_ISINT(a))
        return a;

    return escm_real_make(e, fun(escm_number_rval(a)));
}

static inline escm_atom *
exemath(escm *e, escm_atom *args, double (*fun)(double))
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);
    a = (ESCM_ISINT(atom)) ? (double) escm_number_ival(atom) :
        escm_number_rval(atom);

    return escm_real_make(e, fun(a));
}
