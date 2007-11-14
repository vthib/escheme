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

static escm_number *realtorat(double);

static void number_free(escm_number *);
static void number_print(escm *, escm_number *, escm_output *, int);
static int number_equal(escm *, escm_number *, escm_number *, int);
static int number_parsetest(escm *, int);
static escm_atom *number_parse(escm *);

static escm_number *inputtonumber(escm_input *, int);
static escm_number *getreal(escm_input *, char **, int);
#if 0
static long pgcd(long, long);
# ifdef ESCM_USE_STRINGS
static char *bintostr(long);
# endif
#endif
static inline int isnumber(int);

static escm_number *extoinex(escm_number *);
static escm_number *inextoex(escm_number *);
static escm_number *dupl(escm_number *);

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

    if (!escm_type_ison(ESCM_TYPE_BOOL)) {
	e->FALSE = makeatom(e, makeint(0));
	e->TRUE = makeatom(e, makeint(1));
    }

    (void) escm_procedure_new(e, "exact->inexact", 1, 2, escm_exact_to_inexact,
			      NULL);
    (void) escm_procedure_new(e, "inexact->exact", 1, 2, escm_inexact_to_exact,
			      NULL);

    (void) escm_procedure_new(e, "+", 0, -1, escm_add, NULL);
    (void) escm_procedure_new(e, "-", 1, -1, escm_sub, NULL);
    (void) escm_procedure_new(e, "*", 0, -1, escm_mul, NULL);
    (void) escm_procedure_new(e, "/", 1, -1, escm_div, NULL);

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

#if 0

    (void) escm_procedure_new(e, "zero?", 1, 1, escm_zero_p, NULL);
    (void) escm_procedure_new(e, "positive?", 1, 1, escm_positive_p, NULL);
    (void) escm_procedure_new(e, "negative?", 1, 1, escm_negative_p, NULL);
    (void) escm_procedure_new(e, "odd?", 1, 1, escm_odd_p, NULL);
    (void) escm_procedure_new(e, "even?", 1, 1, escm_even_p, NULL);

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

#ifdef ESCM_USE_STRINGS
    (void) escm_procedure_new(e, "number->string", 1, 2, escm_number_to_string,
			      NULL);
    (void) escm_procedure_new(e, "string->number", 1, 2, escm_string_to_number,
			      NULL);
#endif

    (void) escm_procedure_new(e, "=", 2, -1, escm_eq, NULL);
    (void) escm_procedure_new(e, "<", 2, -1, escm_lt, NULL);
    (void) escm_procedure_new(e, ">", 2, -1, escm_gt, NULL);
    (void) escm_procedure_new(e, "<=", 2, -1, escm_le, NULL);
    (void) escm_procedure_new(e, ">=", 2, -1, escm_ge, NULL);
#endif
}

size_t
escm_cnumber_tget(void)
{
    return numbertype;
}

escm_atom *
escm_cint_make(escm *e, long i)
{
    escm_number *n;

    n = xmalloc(sizeof *n);
    n->type = ESCM_INTEGER, n->d.i = i;
    n->exact = 1;

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

    if (args) {
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

    if (args) {
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
escm_number_p(escm *e, escm_atom *args, void *data)
{
    escm_atom *a;

    a = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(a), a, e);

    return (((escm_number *) a->ptr)->type <= ((escm_intptr) data - 1)) ?
	e->TRUE : e->FALSE;
}

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

	/* TODO */
    case ESCM_COMPLEX:
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

	/* TODO */
    case ESCM_COMPLEX:
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

	/* TODO */
    case ESCM_COMPLEX:
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
	default: break;
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
	    a->d.cpx.re = dest, *destptr = NULL;
	    a->d.cpx.im = makeint(0);
	    break;
	default: break;
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
    if (src->d.i >= 0) {
	if ((LONG_MAX - src->d.i) < (*dest)->d.i) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    } else {
	if ((LONG_MIN - src->d.i) > (*dest)->d.i) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    }

    (*dest)->d.i += src->d.i; /* XXX: bignums */
}

static void
subint(escm *e, escm_number **dest, escm_number *src)
{
    if (src->d.i >= 0) {
	if ((LONG_MIN + src->d.i) > (*dest)->d.i) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    } else {
	if ((LONG_MAX + src->d.i) < (*dest)->d.i) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    }

    (*dest)->d.i -= src->d.i; /* XXX: bignums */
}

static void
mulint(escm *e, escm_number **dest, escm_number *src)
{
    if (src->d.i > 1) {
	if ((LONG_MAX / src->d.i) < (*dest)->d.i) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    } else {
	if ((LONG_MIN / src->d.i) > (*dest)->d.i) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
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
    if (DBL_GE(src->d.real, 0.)) {
	if (DBL_LT((DBL_MAX - src->d.real), (*dest)->d.real)) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    } else {
	if (DBL_GT((DBL_MIN - src->d.real), (*dest)->d.real)) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    }

    (*dest)->d.real += src->d.real; /* XXX: bigreals */
}

static void
subreal(escm *e, escm_number **dest, escm_number *src)
{
    if (DBL_GE(src->d.real, 0.)) {
	if (DBL_GT((DBL_MIN + src->d.real), (*dest)->d.real)) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    } else {
	if (DBL_LT((DBL_MAX + src->d.real), (*dest)->d.real)) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    }

    (*dest)->d.real -= src->d.real;
}

static void
mulreal(escm *e, escm_number **dest, escm_number *src)
{
    if (DBL_GT(src->d.real, 1.)) {
	if (DBL_LT((DBL_MAX / src->d.real), (*dest)->d.real)) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
    } else if (DBL_LT(src->d.real, -1.)) {
	if (DBL_GT((DBL_MIN / src->d.real), (*dest)->d.real)) {
	    escm_printf(e->errp, "number overflow.\n");
	    return;
	}
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
	if (DBL_GT(src->d.real, 0.)) {
	    if (DBL_LT((DBL_MAX * src->d.real), (*dest)->d.real)) {
		escm_printf(e->errp, "number overflow.\n");
		return;
	    }
	} else {
	    if (DBL_LT((DBL_MIN * src->d.real), (*dest)->d.real)) {
		escm_printf(e->errp, "number overflow.\n");
		return;
	    }
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
	/* XXX: ugly */
    case ESCM_RATIONAL: return DBL_EQ(n1->d.rat.n / n1->d.rat.d,
				      n2->d.rat.n / n2->d.rat.d);
    case ESCM_COMPLEX:
	return (number_equal(e, n1->d.cpx.re, n2->d.cpx.re, lvl) &&
		number_equal(e, n1->d.cpx.im, n2->d.cpx.im, lvl));
    }

    /* not supposed to happen but prevents a warning */
    return 0;
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

    n = inputtonumber(e->input, 10);
    if (!n)
	return NULL;
    return escm_atom_new(e, numbertype, n);
}

static escm_number *
inputtonumber(escm_input *input, int radix)
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
	    escm_input_print(input, "unknown character #%c.", c);
	    return NULL;
	}
    }
    escm_input_ungetc(input, c);

    str = escm_input_getstr_fun(input, isnumber, 1);
    p = str;
    if (strchr(str, 'i')) {
	escm_number *cpx;

	cpx = xmalloc(sizeof *cpx);
	cpx->type = ESCM_COMPLEX;
	if (*(str + 1) == 'i') {
	    if (*(str + 2) != '\0') {
		escm_input_print(input, "complex number must end with a i.\n");
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
	cpx->d.cpx.re = getreal(input, &p, radix);
	if (!cpx->d.cpx.re)
	    goto cpxbad;

	if (*p != '-' && *p != '+') {	
	    escm_input_print(input, "number parse error.\n");
	    number_free(cpx->d.cpx.re);
	    goto cpxbad;
	}

	cpx->d.cpx.im = getreal(input, &p, radix);
	if (*p != 'i' || *(p + 1) != '\0') {
	    escm_input_print(input, "complex number must end with a i.\n");
	    number_free(cpx->d.cpx.re);
	    goto cpxbad;
	}

	if ((cpx->d.cpx.im->type == ESCM_INTEGER && cpx->d.cpx.im->d.i == 0) ||
	    (cpx->d.cpx.im->type == ESCM_REAL &&
	     DBL_EQ(cpx->d.cpx.re->d.real, 0.)) ||
	    (cpx->d.cpx.im->type == ESCM_RATIONAL &&
	     cpx->d.cpx.im->d.rat.n == 0)) {
		escm_number *a;

		number_free(cpx->d.cpx.im);
		a = cpx->d.cpx.re, free(cpx);
		free(str);
		return a;
	    }

	free(str);
	cpx->exact = (cpx->d.cpx.re->exact | cpx->d.cpx.re->exact);
	if (exact != 2) {
	    if (exact == 1 && cpx->exact != 1) {
		escm_number *tmp;

		tmp = inextoex(cpx);
		free(cpx), cpx = tmp;
	    } else if (exact == 0 && (cpx->d.cpx.re->exact ||
				      cpx->d.cpx.im->exact)) {

		escm_number *tmp;

		tmp = extoinex(cpx);
		free(cpx), cpx = tmp;
	    }
	}
	return cpx;

    cpxbad:
	free(cpx);
	free(str);
	return NULL;
    }

    a = getreal(input, &p, radix);
    free(str);
    if (exact == 1 && a->exact != 1) {
	escm_number *tmp;

	tmp = inextoex(a);
	free(a), a = tmp;
    } else if (exact == 0 && a->exact == 1) {
	escm_number *tmp;

	tmp = extoinex(a);
	free(a), a = tmp;
    }
    return a;
}

static escm_number *
getreal(escm_input *input, char **p, int radix)
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
	double e;

	e = strtod(*p, p);
	if (DBL_EQ(floor(e), e)) {
	    escm_number *n;

	    n = makeint((long) e);
	    n->exact = 0;
	    return n;
	}
	return makereal(e);
    } else if (strchr(*p, '/')) {
	long n, d;

	n = strtol(*p, p, radix);
	if (*(*p + 1) == '-') {
	    escm_input_print(input, "the denominator must be positive.\n");
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

#if 0
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
#endif /* 0 */

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
    }
    return n;
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
    }
    return n;
}

static escm_number *
dupl(escm_number *n)
{
    escm_number *a;

    a = xmalloc(sizeof *n);
    a->type = n->type, a->exact = n->exact;

    if (n->type == ESCM_COMPLEX) {
	a->d.cpx.re = dupl(a->d.cpx.re);
	a->d.cpx.im = dupl(a->d.cpx.im);
    } else
	memcpy(&(a->d), &(n->d), sizeof n->d);

    return a;
}
