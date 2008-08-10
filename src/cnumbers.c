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
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>

#include "escheme.h"
#include "cnumbers.h"

static unsigned long numbertype = 0;

static void harmonize(escm_number **, escm_number **, int *);

#define makeatom(e, n) escm_atom_new(e, numbertype, n)

static escm_number *makeint(long);
static void addint(escm *, escm_number **, escm_number *);
static void subint(escm *, escm_number **, escm_number *);
static void mulint(escm *, escm_number **, escm_number *);
static void divint(escm *, escm_number **, escm_number *);

static escm_number *makereal(double);
static void addreal(escm *, escm_number **, escm_number *);
static void subreal(escm *, escm_number **, escm_number *);
static void mulreal(escm *, escm_number **, escm_number *);
static void divreal(escm *, escm_number **, escm_number *);

static escm_number *makerat(long, long);
static void addrat(escm *, escm_number **, escm_number *);
static void subrat(escm *, escm_number **, escm_number *);
static void mulrat(escm *, escm_number **, escm_number *);
static void divrat(escm *, escm_number **, escm_number *);

static void addcpx(escm *, escm_number **, escm_number *);
static void subcpx(escm *, escm_number **, escm_number *);
static void mulcpx(escm *, escm_number **, escm_number *);
static void divcpx(escm *, escm_number **, escm_number *);

static escm_number *realtorat(double);
static double numbertoreal(escm_number *);

static void number_free(escm_number *);
static void number_print(escm *, escm_number *, escm_output *, int);
static int number_equal(escm *, escm_number *, escm_number *, int);
static int number_parsetest(escm *, int);
static escm_atom *number_parse(escm *);

static escm_number *inputtonumber(escm *, escm_input *, int);
static escm_number *getreal(escm *, escm_input *, char **, int);

static inline int isnumber(int);
static inline int isnegative(escm_number *);

static escm_number *extoinex(escm_number *);
static escm_number *inextoex(escm_number *);
static escm_number *dupl(escm_number *);
static long pgcd(long, long);

#ifdef ESCM_USE_STRINGS
static char *bintostr(long);
static char *numbertostr(escm *, escm_number *, int);
#endif

void
escm_cnumbers_init(escm *e)
{
    escm_type *t;

    t = xcalloc(1, sizeof *t);
    t->ffree = (Escm_Fun_Free) number_free;
    t->d.c.fprint = (Escm_Fun_Print) number_print;
    t->d.c.fequal = (Escm_Fun_Equal) number_equal;
    t->d.c.fparsetest = number_parsetest;
    t->d.c.fparse = number_parse;

    numbertype = escm_type_add(e, t);

    (void) escm_procedure_new(e, "number?", 1, 1, (Escm_Fun_Prim) escm_number_p,
                              (void *) (ESCM_COMPLEX + 1));
    (void) escm_procedure_new(e, "integer?", 1, 1, (Escm_Fun_Prim)escm_number_p,
                              (void *) (ESCM_INTEGER + 1));
    (void) escm_procedure_new(e, "real?", 1, 1, (Escm_Fun_Prim) escm_number_p,
                              (void *) (ESCM_REAL + 1));
    (void) escm_procedure_new(e, "rational?", 1, 1,
                              (Escm_Fun_Prim) escm_number_p,
                              (void *) (ESCM_RATIONAL + 1));
    (void) escm_procedure_new(e, "complex?", 1, 1, (Escm_Fun_Prim)escm_number_p,
                              (void *) (ESCM_COMPLEX + 1));

    (void) escm_procedure_new(e, "exact?", 1, 1, escm_exact_p, NULL);
    (void) escm_procedure_new(e, "inexact?", 1, 1, escm_inexact_p, NULL);

    (void) escm_procedure_new(e, "=", 2, -1, escm_eq, NULL);
    (void) escm_procedure_new(e, "<", 2, -1, escm_lt, NULL);
    (void) escm_procedure_new(e, ">", 2, -1, escm_gt, NULL);
    (void) escm_procedure_new(e, "<=", 2, -1, escm_le, NULL);
    (void) escm_procedure_new(e, ">=", 2, -1, escm_ge, NULL);

    (void) escm_procedure_new(e, "+", 0, -1, escm_add, NULL);
    (void) escm_procedure_new(e, "-", 1, -1, escm_sub, NULL);
    (void) escm_procedure_new(e, "*", 0, -1, escm_mul, NULL);
    (void) escm_procedure_new(e, "/", 1, -1, escm_div, NULL);

    (void) escm_procedure_new(e, "quotient", 2, 2, escm_quotient, NULL);
    (void) escm_procedure_new(e, "remainder", 2, 2, escm_remainder, NULL);
    (void) escm_procedure_new(e, "modulo", 2, 2, escm_modulo, NULL);

    (void) escm_procedure_new(e, "gcd", 0, -1, escm_gcd, NULL);
    (void) escm_procedure_new(e, "lcm", 0, -1, escm_lcm, NULL);

    (void) escm_procedure_new(e, "numerator", 1, 1, escm_numerator, NULL);
    (void) escm_procedure_new(e, "denominator", 1, 1, escm_denominator, NULL);

#ifdef ESCM_USE_MATH
    (void) escm_procedure_new(e, "floor", 1, 1, escm_floor, NULL);
    (void) escm_procedure_new(e, "ceiling", 1, 1, escm_ceiling, NULL);
    (void) escm_procedure_new(e, "truncate", 1, 1, escm_truncate, NULL);
    (void) escm_procedure_new(e, "round", 1, 1, escm_round, NULL);

    (void) escm_procedure_new(e, "exp", 1, 1, escm_exp, NULL);
    (void) escm_procedure_new(e, "log", 1, 1, escm_log, NULL);
    (void) escm_procedure_new(e, "sin", 1, 1, escm_sin, NULL);
    (void) escm_procedure_new(e, "cos", 1, 1, escm_cos, NULL);
    (void) escm_procedure_new(e, "tan", 1, 1, escm_tan, NULL);
    (void) escm_procedure_new(e, "asin", 1, 1, escm_asin, NULL);
    (void) escm_procedure_new(e, "acos", 1, 1, escm_acos, NULL);
    (void) escm_procedure_new(e, "atan", 1, 2, escm_atan, NULL);

    (void) escm_procedure_new(e, "sqrt", 1, 1, escm_sqrt, NULL);
    (void) escm_procedure_new(e, "expt", 2, 2, escm_expt, NULL);
#endif

    (void) escm_procedure_new(e, "exact->inexact", 1, 2, escm_exact_to_inexact,
                              NULL);
    (void) escm_procedure_new(e, "inexact->exact", 1, 2, escm_inexact_to_exact,
                              NULL);

#ifdef ESCM_USE_STRINGS
    (void) escm_procedure_new(e, "number->string", 1, 2, escm_number_to_string,
                              NULL);
    (void) escm_procedure_new(e, "string->number", 1, 2, escm_string_to_number,
                              NULL);
#endif
}

size_t
escm_cnumber_tget(void)
{
    return numbertype;
}

escm_atom *
escm_cint_make(escm *e, long i, int exact)
{
    escm_number *n;

    n = xmalloc(sizeof *n);
    n->type = ESCM_INTEGER, n->d.i = i;
    n->exact = exact;

    return escm_atom_new(e, numbertype, n);
}

escm_atom *
escm_creal_make(escm *e, double r)
{
    escm_number *n;

    n = xmalloc(sizeof *n);
    n->type = ESCM_REAL, n->d.real = r;
    n->exact = 0;

    return escm_atom_new(e, numbertype, n);
}

escm_atom *
escm_number_p(escm *e, escm_atom *args, void *data)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);

    return (escm_cnumber_val(a)->type <= (size_t) ((escm_intptr) data - 1)) ?
        e->TRUE : e->FALSE;
}

escm_atom *
escm_exact_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);

    return escm_cnumber_val(a)->exact ? e->TRUE : e->FALSE;
}

escm_atom *
escm_inexact_p(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);

    return escm_cnumber_val(a)->exact ? e->FALSE : e->TRUE;
}

escm_atom *
escm_add(escm *e, escm_atom *args)
{
    escm_number *a;
    escm_atom *atom;

    a = makeint(0);

    while ((atom = escm_cons_pop(e, &args)) != NULL) {
        escm_assert1(ESCM_ISNUMBER(atom), atom, e, free(a));
        escm_number_add(e, &a, atom->ptr);
    }

    return escm_atom_new(e, numbertype, a);
}

escm_atom *
escm_sub(escm *e, escm_atom *args)
{
    escm_number *a;
    escm_atom *atom;

    a = makeint(0);

    atom = escm_cons_pop(e, &args);

    if (args != e->NIL) {
        free(a);
        escm_assert(ESCM_ISNUMBER(atom), atom, e);
        a = dupl(atom->ptr);
        atom = escm_cons_pop(e, &args);
    }

    do {
        escm_assert1(ESCM_ISNUMBER(atom), atom, e, free(a));
        escm_number_sub(e, &a, atom->ptr);

        atom = escm_cons_pop(e, &args);
    } while (atom);

    return escm_atom_new(e, numbertype, a);
}

escm_atom *
escm_mul(escm *e, escm_atom *args)
{
    escm_number *a;
    escm_atom *atom;

    a = makeint(1);

    while ((atom = escm_cons_pop(e, &args)) != NULL) {
        escm_assert1(ESCM_ISNUMBER(atom), atom, e, free(a));
        escm_number_mul(e, &a, atom->ptr);
    }

    return escm_atom_new(e, numbertype, a);
}

escm_atom *
escm_div(escm *e, escm_atom *args)
{
    escm_number *a;
    escm_atom *atom;

    a = makeint(1);

    atom = escm_cons_pop(e, &args);

    if (args != e->NIL) {
        free(a);
        escm_assert(ESCM_ISNUMBER(atom), atom, e);
        a = dupl(atom->ptr);
        atom = escm_cons_pop(e, &args);
    }

    do {
        escm_assert1(ESCM_ISNUMBER(atom), atom, e, free(a));
        escm_number_div(e, &a, atom->ptr);

        atom = escm_cons_pop(e, &args);
    } while (atom);

    return escm_atom_new(e, numbertype, a);
}

escm_atom *
escm_eq(escm *e, escm_atom *args)
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
escm_lt(escm *e, escm_atom *args)
{
    escm_number *a, *b;
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISNUMBER(c), c, e);
        b = c->ptr;

        if (a->type == ESCM_COMPLEX || b->type == ESCM_COMPLEX) {
            escm_error(e, "~s: cannot compared complex numbers.~%",
                       escm_fun(e));
            return e->FALSE;
        }

        if (a->type != b->type) {
            if (a->type == ESCM_RATIONAL) {
                if (!DBL_LT(a->d.rat.n / (double) a->d.rat.d,
                            (b->type == ESCM_REAL) ? b->d.real : b->d.i))
                    return e->FALSE;
            } else if (b->type == ESCM_RATIONAL) {
                if (!DBL_LT((a->type == ESCM_REAL) ? a->d.real : a->d.i,
                            b->d.rat.n / (double) b->d.rat.d))
                    return e->FALSE;
            } else {
                if (!DBL_LT((a->type == ESCM_REAL) ? a->d.real : a->d.i,
                            (b->type == ESCM_REAL) ? b->d.real : b->d.i))
                    return e->FALSE;
            }
        } else {
            switch (a->type) {
            case ESCM_INTEGER:
                if (!(a->d.i < b->d.i))
                    return e->FALSE;
                break;
            case ESCM_REAL:
                if (!DBL_LT(a->d.real, b->d.real))
                    return e->FALSE;
                break;
            case ESCM_RATIONAL:
                if (a->d.rat.d == b->d.rat.d) {
                    if (!(a->d.rat.n < b->d.rat.n))
                        return e->FALSE;
                } else {
                    if (!DBL_LT(a->d.rat.n / (double) a->d.rat.d,
                                b->d.rat.n / (double) b->d.rat.n))
                        return e->FALSE;
                }
            default:
                break;
            }
        }

        a = b;
    }

    return e->TRUE;
}

escm_atom *
escm_gt(escm *e, escm_atom *args)
{
    escm_number *a, *b;
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISNUMBER(c), c, e);
        b = c->ptr;

        if (a->type == ESCM_COMPLEX || b->type == ESCM_COMPLEX) {
            escm_error(e, "~s: cannot compared complex numbers.~%",
                       escm_fun(e));
            return e->FALSE;
        }

        if (a->type != b->type) {
            if (a->type == ESCM_RATIONAL) {
                if (!DBL_GT(a->d.rat.n / (double) a->d.rat.d,
                            (b->type == ESCM_REAL) ? b->d.real : b->d.i))
                    return e->FALSE;
            } else if (b->type == ESCM_RATIONAL) {
                if (!DBL_GT((a->type == ESCM_REAL) ? a->d.real : a->d.i,
                            b->d.rat.n / (double) b->d.rat.d))
                    return e->FALSE;
            } else {
                if (!DBL_GT((a->type == ESCM_REAL) ? a->d.real : a->d.i,
                            (b->type == ESCM_REAL) ? b->d.real : b->d.i))
                    return e->FALSE;
            }
        } else {
            switch (a->type) {
            case ESCM_INTEGER:
                if (!(a->d.i > b->d.i))
                    return e->FALSE;
                break;
            case ESCM_REAL:
                if (!DBL_GT(a->d.real, b->d.real))
                    return e->FALSE;
                break;
            case ESCM_RATIONAL:
                if (a->d.rat.d == b->d.rat.d) {
                    if (!(a->d.rat.n > b->d.rat.n))
                        return e->FALSE;
                } else {
                    if (!DBL_GT(a->d.rat.n / (double) a->d.rat.d,
                                b->d.rat.n / (double) b->d.rat.n))
                        return e->FALSE;
                }
            default:
                break;
            }
        }

        a = b;
    }

    return e->TRUE;
}

escm_atom *
escm_le(escm *e, escm_atom *args)
{
    escm_number *a, *b;
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISNUMBER(c), c, e);
        b = c->ptr;

        if (a->type == ESCM_COMPLEX || b->type == ESCM_COMPLEX) {
            escm_error(e, "~s: cannot compared complex numbers.~%",
                       escm_fun(e));
            return e->FALSE;
        }

        if (a->type != b->type) {
            if (a->type == ESCM_RATIONAL) {
                if (!DBL_LE(a->d.rat.n / (double) a->d.rat.d,
                            (b->type == ESCM_REAL) ? b->d.real : b->d.i))
                    return e->FALSE;
            } else if (b->type == ESCM_RATIONAL) {
                if (!DBL_LE((a->type == ESCM_REAL) ? a->d.real : a->d.i,
                            b->d.rat.n / (double) b->d.rat.d))
                    return e->FALSE;
            } else {
                if (!DBL_LE((a->type == ESCM_REAL) ? a->d.real : a->d.i,
                            (b->type == ESCM_REAL) ? b->d.real : b->d.i))
                    return e->FALSE;
            }
        } else {
            switch (a->type) {
            case ESCM_INTEGER:
                if (!(a->d.i <= b->d.i))
                    return e->FALSE;
                break;
            case ESCM_REAL:
                if (!DBL_LE(a->d.real, b->d.real))
                    return e->FALSE;
                break;
            case ESCM_RATIONAL:
                if (a->d.rat.d == b->d.rat.d) {
                    if (!(a->d.rat.n <= b->d.rat.n))
                        return e->FALSE;
                } else {
                    if (!DBL_LE(a->d.rat.n / (double) a->d.rat.d,
                                b->d.rat.n / (double) b->d.rat.n))
                        return e->FALSE;
                }
            default:
                break;
            }
        }

        a = b;
    }

    return e->TRUE;
}

escm_atom *
escm_ge(escm *e, escm_atom *args)
{
    escm_number *a, *b;
    escm_atom *c;

    c = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(c), c, e);
    a = c->ptr;

    for (c = escm_cons_pop(e, &args); c; c = escm_cons_pop(e, &args)) {
        escm_assert(ESCM_ISNUMBER(c), c, e);
        b = c->ptr;

        if (a->type == ESCM_COMPLEX || b->type == ESCM_COMPLEX) {
            escm_error(e, "~s: cannot compared complex numbers.~%",
                       escm_fun(e));
            return e->FALSE;
        }

        if (a->type != b->type) {
            if (a->type == ESCM_RATIONAL) {
                if (!DBL_GE(a->d.rat.n / (double) a->d.rat.d,
                            (b->type == ESCM_REAL) ? b->d.real : b->d.i))
                    return e->FALSE;
            } else if (b->type == ESCM_RATIONAL) {
                if (!DBL_GE((a->type == ESCM_REAL) ? a->d.real : a->d.i,
                            b->d.rat.n / (double) b->d.rat.d))
                    return e->FALSE;
            } else {
                if (!DBL_GE((a->type == ESCM_REAL) ? a->d.real : a->d.i,
                            (b->type == ESCM_REAL) ? b->d.real : b->d.i))
                    return e->FALSE;
            }
        } else {
            switch (a->type) {
            case ESCM_INTEGER:
                if (!(a->d.i >= b->d.i))
                    return e->FALSE;
                break;
            case ESCM_REAL:
                if (!DBL_GE(a->d.real, b->d.real))
                    return e->FALSE;
                break;
            case ESCM_RATIONAL:
                if (a->d.rat.d == b->d.rat.d) {
                    if (!(a->d.rat.n >= b->d.rat.n))
                        return e->FALSE;
                } else {
                    if (!DBL_GE(a->d.rat.n / (double) a->d.rat.d,
                                b->d.rat.n / (double) b->d.rat.n))
                        return e->FALSE;
                }
            default:
                break;
            }
        }

        a = b;
    }

    return e->TRUE;
}

escm_atom *
escm_quotient(escm *e, escm_atom *args)
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
    
    return escm_cint_make(e, escm_number_ival(n) / escm_number_ival(m),
                         escm_cnumber_exactp(n) && escm_cnumber_exactp(m));
}

escm_atom *
escm_remainder(escm *e, escm_atom *args)
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

    return escm_cint_make(e, escm_number_ival(n) % escm_number_ival(m),
                         escm_cnumber_exactp(n) && escm_cnumber_exactp(m));
}

escm_atom *
escm_modulo(escm *e, escm_atom *args)
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

    return escm_cint_make(e, res, escm_cnumber_exactp(n) &&
                          escm_cnumber_exactp(m));
}

escm_atom *
escm_gcd(escm *e, escm_atom *args)
{
    escm_atom *n1, *n2;
    long a, b;
    int exact = 1;

    n1 = escm_cons_pop(e, &args);
    if (!n1)
        return escm_int_make(e, 0);
    escm_assert(ESCM_ISINT(n1), n1, e);
    a = escm_number_ival(n1);
    exact &= escm_cnumber_exactp(n1);

    n2 = escm_cons_pop(e, &args);
    if (!n2)
        return escm_cint_make(e, escm_number_ival(n1), exact);
    escm_assert(ESCM_ISINT(n2), n2, e);
    b = escm_number_ival(n2);
    exact &= escm_cnumber_exactp(n2);

    for (;;) {
        if (b == 0)
            return escm_cint_make(e, a, exact);
        if (a == 0)
            return escm_cint_make(e, b, exact);

        a = pgcd(a, b);

        n2 = escm_cons_pop(e, &args);
        if (!n2)
            return escm_cint_make(e, a, exact);

        escm_assert(ESCM_ISINT(n2), n2, e);
        b = escm_number_ival(n2);
        exact &= escm_cnumber_exactp(n2);
    }
}

escm_atom *
escm_lcm(escm *e, escm_atom *args)
{
    escm_atom *n1, *n2;
    long a, b, c;
    int exact = 1;

    n1 = escm_cons_pop(e, &args);
    if (!n1)
        return escm_int_make(e, 1);

    escm_assert(ESCM_ISINT(n1), n1, e);
    a = ABS(escm_number_ival(n1));
    exact &= escm_cnumber_exactp(n1);

    n2 = escm_cons_pop(e, &args);
    if (!n2)
        return escm_cint_make(e, escm_number_ival(n1), exact);

    escm_assert(ESCM_ISINT(n2), n2, e);
    b = ABS(escm_number_ival(n2));
    exact &= escm_cnumber_exactp(n2);

    for (;;) {
        c = pgcd(a, b);

        if ((LONG_MAX / b) < a) {
            escm_error(e, "~s: integer overflow.~%", escm_fun(e));
            escm_abort(e);
        }
        a = a*b / c;

        n2 = escm_cons_pop(e, &args);
        if (!n2)
            return escm_cint_make(e, a, exact);

        escm_assert(ESCM_ISINT(n2), n2, e);
        b = ABS(escm_number_ival(n2));
        exact &= escm_cnumber_exactp(n2);
    }
}

escm_atom *
escm_numerator(escm *e, escm_atom *args)
{
    escm_atom *n, *ret;
    double a;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(n) && !ESCM_ISCOMPLEX(n), n, e);

    if (ESCM_ISINT(n))
        return n;
    if (ESCM_ISRATIONAL(n)) {
        ret = escm_int_make(e, escm_cnumber_val(n)->d.rat.n);
        escm_cnumber_val(ret)->exact = escm_cnumber_exactp(n);
        return ret;
    }

    a = escm_number_rval(n);

    a = escm_number_rval(n);
    while (!DBL_EQ(a, floor(a))) {
        if (DBL_LT((LONG_MAX / 2), a)) {
            escm_error(e, "~s: integer overflow.~%", escm_fun(e));
            escm_abort(e);
        }
        a *= 2;
    }

    ret = escm_int_make(e, (long) a);
    escm_cnumber_val(ret)->exact = escm_cnumber_exactp(n);
    return ret;
}

escm_atom *
escm_denominator(escm *e, escm_atom *args)
{
    escm_atom *n, *ret;
    double a;
    long b;

    n = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(n) && !ESCM_ISCOMPLEX(n), n, e);

    if (ESCM_ISINT(n))
        return n;
    if (ESCM_ISRATIONAL(n)) {
        ret = escm_int_make(e, escm_cnumber_val(n)->d.rat.d);
        escm_cnumber_val(ret)->exact = escm_cnumber_exactp(n);
        return ret;
    }

    b = 1;
    a = escm_number_rval(n);
    while (!DBL_EQ(a, floor(a))) {
        if (DBL_LT((LONG_MAX / 2), a) || DBL_LT((LONG_MAX / 2), b)) {
            escm_error(e, "~s: integer overflow.~%", escm_fun(e));
            escm_abort(e);
        }
        a *= 2, b *= 2;
    }

    ret = escm_int_make(e, b);
    escm_cnumber_val(ret)->exact = escm_cnumber_exactp(n);
    return ret;
}

#ifdef ESCM_USE_MATH
escm_atom *
escm_floor(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a) && !ESCM_ISCOMPLEX(a), a, e);

    if (ESCM_ISINT(a))
        return a;
    return escm_cint_make(e, (long) floor(numbertoreal(a->ptr)),
                          ESCM_ISRATIONAL(a));
}

escm_atom *
escm_ceiling(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a) && !ESCM_ISCOMPLEX(a), a, e);

    if (ESCM_ISINT(a))
        return a;
    return escm_cint_make(e, (long) ceil(numbertoreal(a->ptr)),
                          ESCM_ISRATIONAL(a));
}

escm_atom *
escm_truncate(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a) && !ESCM_ISCOMPLEX(a), a, e);

    if (ESCM_ISINT(a))
        return a;
# ifdef ESCM_USE_C99
    return escm_cint_make(e, (long) trunc(numbertoreal(a->ptr)),
                          ESCM_ISRATIONAL(a));
# else
    {
        double r;

        r = numbertoreal(a->ptr);
        if (DBL_GE(r, 0.))
            return escm_cint_make(e, (long) floor(r), ESCM_ISRATIONAL(a));
        else
            return escm_cint_make(e, (long) ceil(r), ESCM_ISRATIONAL(a));
    }
# endif
}

escm_atom *
escm_round(escm *e, escm_atom *args)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a) && !ESCM_ISCOMPLEX(a), a, e);

    if (ESCM_ISINT(a))
        return a;
    return escm_cint_make(e, (long) xround(numbertoreal(a->ptr)),
                          ESCM_ISRATIONAL(a));
}

escm_atom *
escm_exp(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    return escm_real_make(e, exp(numbertoreal(atom->ptr)));
}

escm_atom *
escm_log(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    return escm_real_make(e, log(numbertoreal(atom->ptr)));
}

escm_atom *
escm_sin(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    return escm_real_make(e, sin(numbertoreal(atom->ptr)));
}

escm_atom *
escm_cos(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    return escm_real_make(e, cos(numbertoreal(atom->ptr)));
}

escm_atom *
escm_tan(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    return escm_real_make(e, tan(numbertoreal(atom->ptr)));
}

escm_atom *
escm_asin(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    return escm_real_make(e, asin(numbertoreal(atom->ptr)));
}

escm_atom *
escm_acos(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    return escm_real_make(e, acos(numbertoreal(atom->ptr)));
}

escm_atom *
escm_atan(escm *e, escm_atom *args)
{
    escm_atom *atom;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    return escm_real_make(e, atan(numbertoreal(atom->ptr)));
}

escm_atom *
escm_sqrt(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    a = sqrt(numbertoreal(atom->ptr));

    if (DBL_EQ(a, floor(a))) /* exact */
        return escm_int_make(e, (long) a);
    else
        return escm_real_make(e, a);
}

escm_atom *
escm_expt(escm *e, escm_atom *args)
{
    escm_atom *atom;
    double a;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);
    a = numbertoreal(atom->ptr);

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom) && !ESCM_ISCOMPLEX(atom), atom, e);

    a = pow(a, numbertoreal(atom->ptr));

    if (DBL_EQ(a, floor(a))) /* exact */
        return escm_int_make(e, (long) a);
    else
        return escm_real_make(e, a);
}
#endif

escm_atom *
escm_exact_to_inexact(escm *e, escm_atom *args)
{
    escm_atom *atom;
    escm_number *n;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);

    n = atom->ptr;
    if (!n->exact)
        return atom;

    return makeatom(e, extoinex(n));
}

escm_atom *
escm_inexact_to_exact(escm *e, escm_atom *args)
{
    escm_atom *atom;
    escm_number *n;

    atom = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(atom), atom, e);

    n = atom->ptr;
    if (n->exact)
        return atom;

    return makeatom(e, inextoex(n));
}

#ifdef ESCM_USE_STRINGS
escm_atom *
escm_number_to_string(escm *e, escm_atom *args)
{
    escm_atom *a, *b;
    char *str;
    int radix;

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

    str = numbertostr(e, a->ptr, radix);
#ifdef ESCM_USE_UNICODE
    a = escm_ustring_make2(e, str);
#else
    a = escm_astring_make(e, str, strlen(str));
#endif
    free(str);
    return a;
}

escm_atom *
escm_string_to_number(escm *e, escm_atom *args)
{
    escm_atom *a, *b;
    escm_input *input;
    escm_number *number;
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
        number_free(number);
        goto err;
    }

    escm_input_close(input);
    return escm_atom_new(e, numbertype, number);

err:
    escm_input_close(input);
    return e->FALSE;
}
#endif

void
escm_number_add(escm *e, escm_number **dest, escm_number *src)
{
    int freesrc;

    harmonize(dest, &src, &freesrc);

    switch (src->type) {
    case ESCM_INTEGER:
        addint(e, dest, src);
        (*dest)->exact &= src->exact;
        break;
    case ESCM_REAL: addreal(e, dest, src); break;
    case ESCM_RATIONAL: addrat(e, dest, src); break;
    case ESCM_COMPLEX:
        addcpx(e, dest, src);
        (*dest)->exact &= ((*dest)->d.cpx.re->exact | (*dest)->d.cpx.im->exact);
        break;
    }

    if (freesrc)
        free(src);
}

void
escm_number_sub(escm *e, escm_number **dest, escm_number *src)
{
    int freesrc;

    harmonize(dest, &src, &freesrc);

    switch (src->type) {
    case ESCM_INTEGER:
        subint(e, dest, src);
        (*dest)->exact &= src->exact;
        break;
    case ESCM_REAL: subreal(e, dest, src); break;
    case ESCM_RATIONAL: subrat(e, dest, src); break;
    case ESCM_COMPLEX:
        subcpx(e, dest, src);
        (*dest)->exact &= ((*dest)->d.cpx.re->exact | (*dest)->d.cpx.im->exact);
        break;
    }

    if (freesrc)
        free(src);
}

void
escm_number_mul(escm *e, escm_number **dest, escm_number *src)
{
    int freesrc;

    harmonize(dest, &src, &freesrc);

    switch (src->type) {
    case ESCM_INTEGER:
        mulint(e, dest, src);
        (*dest)->exact &= src->exact;
        break;
    case ESCM_REAL: mulreal(e, dest, src); break;
    case ESCM_RATIONAL: mulrat(e, dest, src); break;
    case ESCM_COMPLEX:
        mulcpx(e, dest, src);
        (*dest)->exact &= ((*dest)->d.cpx.re->exact | (*dest)->d.cpx.im->exact);
        break;
    }

    if (freesrc)
        free(src);
}

void
escm_number_div(escm *e, escm_number **dest, escm_number *src)
{
    int freesrc;

    harmonize(dest, &src, &freesrc);

    switch (src->type) {
    case ESCM_INTEGER:
        divint(e, dest, src);
        (*dest)->exact &= src->exact;
        break;
    case ESCM_REAL: divreal(e, dest, src); break;
    case ESCM_RATIONAL: divrat(e, dest, src); break;
    case ESCM_COMPLEX:
        divcpx(e, dest, src);
        (*dest)->exact &= ((*dest)->d.cpx.re->exact | (*dest)->d.cpx.im->exact);
        break;
    }

    if (freesrc)
        free(src);
}

static void
harmonize(escm_number **destptr, escm_number **srcptr, int *freesrc)
{
    escm_number *dest, *src;

    dest = *destptr;
    src = *srcptr;

    *freesrc = 0;
    if (dest->type > src->type) {
        escm_number *a;

        switch (dest->type) {
        case ESCM_REAL: a = makereal((double) src->d.i); break;
        case ESCM_RATIONAL:
        {
            if (src->type == ESCM_INTEGER)
                a = makerat(src->d.i, 1);
            else
                a = realtorat(src->d.real);
            break;
        }
        case ESCM_COMPLEX:
            a = xcalloc(1, sizeof *a);
            a->type = ESCM_COMPLEX;
            a->d.cpx.re = src;
            a->d.cpx.im = makeint(0);
            break;
        default:
            a = NULL;
            break;
        }

        *srcptr = a, *freesrc = 1;
    } else if (dest->type < src->type) {
        escm_number *a;

        switch (src->type) {
        case ESCM_REAL: a = makereal((double) dest->d.i); break;
        case ESCM_RATIONAL:
        {
            if (dest->type == ESCM_INTEGER)
                a = makerat(dest->d.i, 1);
            else
                a = realtorat(dest->d.real);
            break;
        }
        case ESCM_COMPLEX:
            a = xcalloc(1, sizeof *a);
            a->type = ESCM_COMPLEX;
            a->d.cpx.re = dest;
            a->d.cpx.im = makeint(0);
            *destptr = a;
            return;
        default:
            a = NULL;
            break;
        }

        free(dest), *destptr = a;
    }
}

static escm_number *
realtorat(double a)
{
    long b;

    b = 1;
    while (!DBL_EQ(a, floor(a))) /* XXX: bignums */
        a *= 2, b *= 2;

    return makerat((long) a, b);
}

static double
numbertoreal(escm_number *a)
{
    switch (a->type) {
    case ESCM_INTEGER:
        return (double) a->d.i;
    case ESCM_REAL:
        return (double) a->d.real;
    case ESCM_RATIONAL:
        return (double) a->d.rat.n / (double) a->d.rat.d;
    default:
        return 0.;
    }
}

/*--- integers ---*/
static escm_number *
makeint(long a)
{
    escm_number *n;

    n = xmalloc(sizeof *n);
    n->exact = 1, n->type = ESCM_INTEGER;
    n->d.i = a;

    return n;
}

static void
addint(escm *e, escm_number **dest, escm_number *src)
{
    if ((src->d.i >= 0) ? (LONG_MAX - src->d.i) < (*dest)->d.i
        : (LONG_MIN - src->d.i) > (*dest)->d.i) {
        escm_printf(e->errp, "number overflow.\n");
        return;
    }

    (*dest)->d.i += src->d.i; /* XXX: bignums */
}

static void
subint(escm *e, escm_number **dest, escm_number *src)
{
    if ((src->d.i >= 0) ? (LONG_MIN + src->d.i) > (*dest)->d.i
        : (LONG_MAX + src->d.i) < (*dest)->d.i) {
        escm_printf(e->errp, "number overflow.\n");
        return;
    }

    (*dest)->d.i -= src->d.i; /* XXX: bignums */
}

static void
mulint(escm *e, escm_number **dest, escm_number *src)
{
    if ((src->d.i > 1) ? (LONG_MAX / src->d.i) < (*dest)->d.i
        : (LONG_MIN / src->d.i) < (*dest)->d.i) {
        escm_printf(e->errp, "number overflow.\n");
        return;
    }

    (*dest)->d.i *= src->d.i; /* XXX: bignums */
}

static void
divint(escm *e, escm_number **dest, escm_number *src)
{
    if (src->d.i == 0) {
        escm_printf(e->errp, "division by zero.\n");
        return;
    }
    if ((src->d.i > (*dest)->d.i) || ((*dest)->d.i % src->d.i != 0)) {
        long n;

        n = (*dest)->d.i;
        free(*dest);
        *dest = makerat(n, src->d.i);
    } else
        (*dest)->d.i /= src->d.i;
}

/*--- reals ---*/

static escm_number *
makereal(double a)
{
    escm_number *n;

    n = xmalloc(sizeof *n);
    n->exact = 0, n->type = ESCM_REAL;
    n->d.real = a;

    return n;
}

static void
addreal(escm *e, escm_number **dest, escm_number *src)
{
    if (DBL_GE(src->d.real, 0.) ?
        DBL_LT((DBL_MAX - src->d.real), (*dest)->d.real) :
        DBL_GT((DBL_MAX + src->d.real), (*dest)->d.real)) {
        escm_printf(e->errp, "number overflow.\n");
        return;
    }

    (*dest)->d.real += src->d.real; /* XXX: bigreals */
}

static void
subreal(escm *e, escm_number **dest, escm_number *src)
{
    if (DBL_GE(src->d.real, 0.) ?
        DBL_GT((DBL_MAX - src->d.real), (*dest)->d.real) :
        DBL_LT((DBL_MAX + src->d.real), (*dest)->d.real)) {
        escm_printf(e->errp, "number overflow.\n");
        return;
    }

    (*dest)->d.real -= src->d.real;
}

static void
mulreal(escm *e, escm_number **dest, escm_number *src)
{
    if (DBL_GT(src->d.real, 1.) ?
        DBL_LT((DBL_MAX / src->d.real), (*dest)->d.real) :
        DBL_GT((DBL_MAX * src->d.real), (*dest)->d.real)) {
        escm_printf(e->errp, "number overflow.\n");
        return;
    }

    (*dest)->d.real *= src->d.real;
}

static void
divreal(escm *e, escm_number **dest, escm_number *src)
{
    if (DBL_EQ(src->d.real, 0.)) {
        escm_printf(e->errp, "division by zero.\n");
        return;
    }
    if (DBL_LT(src->d.real, 1.) && DBL_GT(src->d.real, -1.)) {
        if (DBL_GT(src->d.real, 0.) ?
            DBL_LT((DBL_MAX * src->d.real), (*dest)->d.real) :
            DBL_LT((DBL_MAX * -src->d.real), (*dest)->d.real)) {
            escm_printf(e->errp, "number overflow.\n");
            return;
        }
    }

    (*dest)->d.real /= src->d.real;
}

/*--- rationals ---*/
static escm_number *
makerat(long n, long d)
{
    escm_number *number;

    number = xmalloc(sizeof *number);
    number->exact = 1, number->type = ESCM_RATIONAL;
    number->d.rat.n = n, number->d.rat.d = d;

    return number;
}

static void
addrat(escm *e, escm_number **dest, escm_number *src)
{
    long d1, d2, f1, f2;

    (void) e;

    d1 = (*dest)->d.rat.d, d2 = src->d.rat.d;
    f1 = f2 = 1;
    while (d1 != d2) {
        if (d1 > d2) {
            f2 = (d1 % d2 == 0) ? d1 / d2 : f2 + 1;
            d2 = src->d.rat.d * f2;
        } else {
            f1 = (d2 % d1 == 0) ? d2 / d1 : f1 + 1;
            d1 = (*dest)->d.rat.d * f1;
        }
    }
    (*dest)->d.rat.n *= f1;
    (*dest)->d.rat.n += (src->d.rat.n * f2);
    (*dest)->d.rat.d *= f1;
}

static void
subrat(escm *e, escm_number **dest, escm_number *src)
{
    long d1, d2, f1, f2;

    (void) e;

    d1 = (*dest)->d.rat.d, d2 = src->d.rat.d;
    f1 = f2 = 1;
    while (d1 != d2) {
        if (d1 > d2) {
            f2 = (d1 % d2 == 0) ? d1 / d2 : f2 + 1;
            d2 = src->d.rat.d * f2;
        } else {
            f1 = (d2 % d1 == 0) ? d2 / d1 : f1 + 1;
            d1 = (*dest)->d.rat.d * f1;
        }
    }
    (*dest)->d.rat.n *= f1;
    (*dest)->d.rat.n -= (src->d.rat.n * f2);
    (*dest)->d.rat.d *= f1;
}

static void
mulrat(escm *e, escm_number **dest, escm_number *src)
{
    (void) e;

    (*dest)->d.rat.n *= src->d.rat.n;
    (*dest)->d.rat.d *= src->d.rat.d;
}

static void
divrat(escm *e, escm_number **dest, escm_number *src)
{
    (void) e;

    (*dest)->d.rat.n *= src->d.rat.d;
    (*dest)->d.rat.d *= src->d.rat.n;
}

/*--- complex ---*/

static void
addcpx(escm *e, escm_number **dest, escm_number *src)
{
    escm_number_add(e, &(*dest)->d.cpx.re, src->d.cpx.re);
    escm_number_add(e, &(*dest)->d.cpx.im, src->d.cpx.im);
}

static void
subcpx(escm *e, escm_number **dest, escm_number *src)
{
    escm_number_sub(e, &(*dest)->d.cpx.re, src->d.cpx.re);
    escm_number_sub(e, &(*dest)->d.cpx.im, src->d.cpx.im);
}

static void
mulcpx(escm *e, escm_number **dest, escm_number *src)
{
    escm_number *a, *b;

    a = dupl((*dest)->d.cpx.re);
    b = dupl((*dest)->d.cpx.im);

    /* real part is ac - bd */
    escm_number_mul(e, &a, src->d.cpx.re);
    escm_number_mul(e, &b, src->d.cpx.im);
    escm_number_sub(e, &a, b); 

    /* imaginary part is ad + bc */
    escm_number_mul(e, &(*dest)->d.cpx.re, src->d.cpx.im);
    escm_number_mul(e, &(*dest)->d.cpx.im, src->d.cpx.re);

    /* set im part */
    escm_number_add(e, &(*dest)->d.cpx.im, (*dest)->d.cpx.re);
    /* set real part */
    memcpy(&((*dest)->d.cpx.re->d), &(a->d), sizeof a->d);
    free(a), free(b);
}

/* (a+bi)/(c+di) = [(a+bi)(c-di)]/(c*c+d*d) */
static void
divcpx(escm *e, escm_number **dest, escm_number *src)
{
    escm_number *a, *b;

    a = dupl(src->d.cpx.re);
    b = dupl(src->d.cpx.im);

    escm_number_mul(e, &a, a);
    escm_number_mul(e, &b, b);
    escm_number_add(e, &a, b); 

    switch (src->d.cpx.im->type) {
    case ESCM_INTEGER:
        src->d.cpx.im->d.i = -src->d.cpx.im->d.i;
        break;
    case ESCM_REAL:
        src->d.cpx.im->d.real = -src->d.cpx.im->d.real;
        break;
    case ESCM_RATIONAL:
        src->d.cpx.im->d.rat.n = -src->d.cpx.im->d.rat.n;
        break;
    default:;
    }

    escm_number_mul(e, dest, src);

    escm_number_div(e, &(*dest)->d.cpx.re, a);
    escm_number_div(e, &(*dest)->d.cpx.im, a);
    free(a), free(b);
}

static void
number_free(escm_number *n)
{
    if (!n)
        return;

    if (n->type == ESCM_COMPLEX) {
        number_free(n->d.cpx.re);
        number_free(n->d.cpx.im);
    }
    free(n);
}

static void
number_print(escm *e, escm_number *number, escm_output *stream, int lvl)
{
    (void) e;
    (void) lvl;

    switch (number->type) {
    case ESCM_INTEGER:
        escm_printf(stream, "%ld", number->d.i);
        if (number->exact == 0)
            escm_putc(stream, '.');
        break;
    case ESCM_REAL:
        escm_printf(stream, "%.15g", number->d.real);
        break;
    case ESCM_RATIONAL:
        escm_printf(stream, "%ld/%ld", number->d.rat.n, number->d.rat.d);
        break;
    case ESCM_COMPLEX:
        number_print(e, number->d.cpx.re, stream, lvl);
        escm_putc(stream, ',');
        number_print(e, number->d.cpx.im, stream, lvl);
        escm_putc(stream, 'i');
        break;
    }
}

static int
number_equal(escm *e, escm_number *n1, escm_number *n2, int lvl)
{
    (void) e;
    (void) lvl;

    if (n1 == n2)
        return 1;
    if (n1->type != n2->type || n1->exact != n2->exact)
        return 0;

    switch (n1->type) {
    case ESCM_INTEGER: return n1->d.i == n2->d.i;
    case ESCM_REAL: return DBL_EQ(n1->d.real, n2->d.real);
    case ESCM_RATIONAL: return DBL_EQ(n1->d.rat.n / (double) n1->d.rat.d,
                                      n2->d.rat.n / (double) n2->d.rat.d);
    case ESCM_COMPLEX:
        return (number_equal(e, n1->d.cpx.re, n2->d.cpx.re, lvl) &&
                number_equal(e, n1->d.cpx.im, n2->d.cpx.im, lvl));
    default:
        return 0;
    }
}

static int
number_parsetest(escm *e, int c)
{
    (void) e;

    if (isdigit(c))
        return 1;
    else if (c == '+' || c == '-') {
        int c2;

        c2 = escm_input_peek(e->input);
        if (c2 == '.')
            return isdigit(escm_input_peek(e->input));
        return (isdigit(c2) || (c2 == 'i'));
    } else if (c == '.')
        return isdigit(escm_input_peek(e->input));
    else if (c == '#')
        return (strchr("boxdei", escm_input_peek(e->input)) != NULL);
    else
        return 0;
}    

static escm_atom *
number_parse(escm *e)
{
    escm_number *n;

    n = inputtonumber(e, e->input, 10);
    if (!n)
        return NULL;
    return escm_atom_new(e, numbertype, n);
}

static escm_number *
inputtonumber(escm *e, escm_input *input, int radix)
{
    escm_number *a;
    char *str;
    char *p;
    int exact;
    int c;

    exact = 2;
    while ((c = escm_input_getc(input)) == '#') {
        switch (escm_input_getc(input)) {
        case 'b': radix = 2; break;
        case 'o': radix = 8; break;
        case 'd': radix = 10; break;
        case 'x': radix = 16; break;
        case 'i': exact = 0; break;
        case 'e': exact = 1; break;
        default:
            escm_parse_print(input, e->errp, "unknown character #%c.\n", c);
            return NULL;
        }
    }
    if (c == EOF)
        return NULL;
    escm_input_ungetc(input, c);

    str = escm_input_getstr_fun(input, isnumber, 1);
    if (!str)
        return NULL;
    p = str;
    if (strchr(str, 'i')) {
        escm_number *cpx;

        cpx = xmalloc(sizeof *cpx);
        cpx->type = ESCM_COMPLEX;
        if (*(str + 1) == 'i') {
            if (*(str + 2) != '\0') {
                escm_parse_print(input, e->errp, "complex number must end with a "
                                 "i.\n");
                goto cpxbad;
            }
            cpx->d.cpx.re = xmalloc(sizeof *cpx->d.cpx.re);
            cpx->d.cpx.re->type = ESCM_INTEGER;
            cpx->d.cpx.re->d.i = 0;
            cpx->d.cpx.re->exact = 1;

            cpx->d.cpx.im = xmalloc(sizeof *cpx->d.cpx.im);
            cpx->d.cpx.im->type = ESCM_INTEGER;
            cpx->d.cpx.im->d.i = (*str == '+') ? 1 : -1;
            cpx->d.cpx.im->exact = 1;

            free(str);
            return cpx;
        }
        cpx->d.cpx.re = getreal(e, input, &p, radix);
        if (!cpx->d.cpx.re)
            goto cpxbad;

        if (*p != '-' && *p != '+') {   
            escm_parse_print(input, e->errp, "expecting a '+' or a '-' before "
                             "the imaginary part.\n");
            number_free(cpx->d.cpx.re);
            goto cpxbad;
        }

        cpx->d.cpx.im = getreal(e, input, &p, radix);
        if (!cpx->d.cpx.im)
            goto cpxbad;
        if (*p == '+' || *p == '-') {       
            cpx->d.cpx.im->d.i = (*p == '+') ? 1 : -1;
            p++;
        }
        if (*p != 'i' || *(p + 1) != '\0') {
            escm_parse_print(input, e->errp, "complex number must end with a i.\n");
            number_free(cpx->d.cpx.im);
            number_free(cpx->d.cpx.re);
            goto cpxbad;
        }

        if ((cpx->d.cpx.im->type == ESCM_INTEGER && cpx->d.cpx.im->d.i == 0) ||
            (cpx->d.cpx.im->type == ESCM_REAL &&
             DBL_EQ(cpx->d.cpx.re->d.real, 0.)) ||
            (cpx->d.cpx.im->type == ESCM_RATIONAL &&
             cpx->d.cpx.im->d.rat.n == 0)) {
                number_free(cpx->d.cpx.im);
                a = cpx->d.cpx.re, free(cpx);
                free(str);
                return a;
            }

        free(str);
        cpx->exact = (cpx->d.cpx.re->exact & cpx->d.cpx.re->exact);
        if (exact != 2) {
            if (exact == 1 && cpx->exact != 1) {
                escm_number *tmp;

                tmp = inextoex(cpx);
                number_free(cpx);
                cpx = tmp;
            } else if (exact == 0 && cpx->exact != 0) {
                escm_number *tmp;

                tmp = extoinex(cpx);
                number_free(cpx);
                cpx = tmp;
            }
        }
        return cpx;

    cpxbad:
        free(cpx);
        free(str);
        return NULL;
    }

    a = getreal(e, input, &p, radix);
    if (exact == 1 && a->exact != 1) {
        escm_number *tmp;

        tmp = inextoex(a);
        free(a), a = tmp;
    } else if (exact == 0 && a->exact == 1) {
        escm_number *tmp;

        tmp = extoinex(a);
        free(a), a = tmp;
    }
    if (*p != '\0') {
        if (input->type == INPUT_FILE)
            input->d.file.car -= strlen(str) - (p - str) - 1;
        else
#ifdef ESCM_USE_UNICODE
            input->d.str.cur = (wchar_t *) input->d.str.str +
                ((p - str + 1) * sizeof (wchar_t));
#else
            input->d.str.cur = (char *) input->d.str.str + (p - str + 1);
#endif
        escm_parse_print(input, e->errp, "Character `%c' unexpected.\n", *p);
        free(str);
        free(a);
        return NULL;
    }
    free(str);
    return a;
}

static escm_number *
getreal(escm *e, escm_input *input, char **p, int radix)
{
    char *exp;
    
    if ((exp = strchr(*p, 's')) != NULL)
        *exp = 'e';
    else if ((exp = strchr(*p, 'f')) != NULL)
        *exp = 'e';
    else if ((exp = strchr(*p, 'd')) != NULL)
        *exp = 'e';
    else if ((exp = strchr(*p, 'l')) != NULL)
        *exp = 'e';
        
    /* this will not work with radix != 10 */
    if (strchr(*p, '.') || strchr(*p, 'e')) {
        double a;

        a = strtod(*p, p);
        if (DBL_EQ(floor(a), a)) {
            escm_number *n;

            n = makeint((long) a);
            n->exact = 0;
            return n;
        }
        return makereal(a);
    } else if (strchr(*p, '/')) {
        long n, d;

        n = strtol(*p, p, radix);
        if (*(*p + 1) == '-') {
            escm_parse_print(input, e->errp,
                             "the denominator must be positive.\n");
            return NULL;
        }
        (*p)++;
        d = strtol(*p, p, radix);
        if (n % d == 0)
            return makeint(n / d);
        return makerat(n, d);
    } else
        return makeint(strtol(*p, p, radix));
}

static int
isnumber(int c)
{
    return (strchr(".+-iesfdl/", c) || isxdigit(c));
}

static inline int
isnegative(escm_number *n)
{
    switch (n->type) {
    case ESCM_INTEGER:
        return n->d.i < 0;
    case ESCM_REAL:
        return DBL_LT(n->d.real, 0.);
    case ESCM_RATIONAL:
        return n->d.rat.n < 0;
    default:
        return 0;
    }
}

static escm_number *
extoinex(escm_number *n)
{
    escm_number *new;

    switch (n->type) {
    case ESCM_INTEGER:
        new = makeint(n->d.i);
        new->exact = 0;
        return new;
    case ESCM_REAL:
        return makereal(n->d.real);
    case ESCM_RATIONAL:
        return makereal(((double) n->d.rat.n) / n->d.rat.d);
    case ESCM_COMPLEX:
        new = xmalloc(sizeof *new);
        new->type = ESCM_COMPLEX, new->exact = 0;
        new->d.cpx.re = extoinex(n->d.cpx.re);
        new->d.cpx.im = extoinex(n->d.cpx.im);
        return new;
    default:
        return n;
    }
}

static escm_number *
inextoex(escm_number *n)
{
    escm_number *new;

    switch (n->type) {
    case ESCM_INTEGER:
        return makeint(n->d.i);
    case ESCM_REAL:
        return realtorat(n->d.real);
    case ESCM_RATIONAL:
        return makerat(n->d.rat.n, n->d.rat.d);
    case ESCM_COMPLEX:
        new = xmalloc(sizeof *new);
        new->type = ESCM_COMPLEX, new->exact = 1;
        new->d.cpx.re = inextoex(n->d.cpx.re);
        new->d.cpx.im = inextoex(n->d.cpx.im);
        return new;
    default:
        return n;
    }
}

static escm_number *
dupl(escm_number *n)
{
    escm_number *a;

    a = xmalloc(sizeof *n);
    a->type = n->type, a->exact = n->exact;

    if (n->type == ESCM_COMPLEX) {
        a->d.cpx.re = dupl(n->d.cpx.re);
        a->d.cpx.im = dupl(n->d.cpx.im);
    } else
        memcpy(&(a->d), &(n->d), sizeof n->d);

    return a;
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

static char *
numbertostr(escm *e, escm_number *n, int radix)
{
    char *str;
    int len, maxlen;

    if (n->type == ESCM_INTEGER) {
        if (radix == 2)
            return bintostr(n->d.i);

        maxlen = 25;
        str = xmalloc(sizeof *str * maxlen);

        switch (radix) {
        case 8:
            len = snprintf(str, maxlen, "%lo", n->d.i);
            break;
        case 16:
            len = snprintf(str, maxlen, "%lx", n->d.i);
            break;
        default:
            len = snprintf(str, maxlen, "%ld", n->d.i);
            break;
        }
        if (len < maxlen && n->exact == 0) {
            str[len] = '.', len++;
            str[len] = '\0';
        }

    } else if (n->type == ESCM_REAL) {
        maxlen = 30;
        str = xmalloc(sizeof *str * maxlen);

        len = snprintf(str, maxlen, "%.15g", n->d.real);
    } else if (n->type == ESCM_RATIONAL) {
        if (radix == 2)
            return bintostr(n->d.i);

        maxlen = 50;
        str = xmalloc(sizeof *str * maxlen);

        switch (radix) {
        case 8:
            len = snprintf(str, maxlen, "%lo/%lo", n->d.rat.n, n->d.rat.d);
            break;
        case 16:
            len = snprintf(str, maxlen, "%lx/%lx", n->d.rat.n, n->d.rat.d);
            break;
        default:
            len = snprintf(str, maxlen, "%ld/%ld", n->d.rat.n, n->d.rat.d);
            break;
        }
    } else {
        char *re, *im;

        re = numbertostr(e, n->d.cpx.re, radix);
        im = numbertostr(e, n->d.cpx.im, radix);

        maxlen = strlen(re) + strlen(im) + 3;
        str = xmalloc(sizeof *str * maxlen);
        if (isnegative(n->d.cpx.im))
            len = snprintf(str, maxlen, "%s-%si", re, im);
        else
            len = snprintf(str, maxlen, "%s+%si", re, im);
        free(re), free(im);
    }

    if (len >= maxlen) { /* output truncated */
        escm_warning(e, "~s: the output was been truncated. The "
                     "read/write invariance may not be respected.~%",
                     escm_fun(e));
        len = maxlen - 1;
    }

    return str;
}
#endif /* ESCM_USE_STRINGS */
