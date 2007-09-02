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
#include <ctype.h>
#include <math.h>

#include "escheme.h"

static size_t numbertype = 0;

static void number_print(escm *, escm_number *, FILE *);
static int number_equal(escm *, escm_number *, escm_number *, unsigned int);
static int number_parsetest(escm *, int);
static escm_atom *number_parse(escm *);

void
escm_numbers_init(escm *e)
{
    escm_type *t;

    t = xmalloc(sizeof *t);
    t->fmark = NULL;
    t->ffree = (Escm_Fun_Free) free;
    t->fprint = (Escm_Fun_Print) number_print;
    t->fequal = (Escm_Fun_Equal) number_equal;
    t->fparsetest = number_parsetest;
    t->fparse = number_parse;
    t->feval = NULL;

    numbertype = escm_type_add(e, t);

#ifdef ESCM_INTBOOL
    {
	escm_number *n;

	n = xmalloc(sizeof *n);
	n->fixnum = 1, n->d.ival = 0;
	e->FALSE = escm_atom_new(e, numbertype, n);

	n = xmalloc(sizeof *n);
	n->fixnum = 1, n->d.ival = 1;
	e->TRUE = escm_atom_new(e, numbertype, n);
    }
#endif

    (void) escm_procedure_new(e, "number?", 1, 1, escm_number_p);
    (void) escm_procedure_new(e, "integer?", 1, 1, escm_integer_p);
    (void) escm_procedure_new(e, "real?", 1, 1, escm_real_p);

    (void) escm_procedure_new(e, "zero?", 1, 1, escm_zero_p);
    (void) escm_procedure_new(e, "positive?", 1, 1, escm_positive_p);
    (void) escm_procedure_new(e, "negative?", 1, 1, escm_negative_p);
    (void) escm_procedure_new(e, "odd?", 1, 1, escm_odd_p);
    (void) escm_procedure_new(e, "even?", 1, 1, escm_even_p);

    (void) escm_procedure_new(e, "+", 0, -1, escm_add);
    (void) escm_procedure_new(e, "-", 1, -1, escm_sub);
    (void) escm_procedure_new(e, "*", 0, -1, escm_mul);
    (void) escm_procedure_new(e, "/", 1, -1, escm_div);

    (void) escm_procedure_new(e, "=", 2, -1, escm_eq);
    (void) escm_procedure_new(e, "<", 2, -1, escm_lt);
    (void) escm_procedure_new(e, ">", 2, -1, escm_gt);
    (void) escm_procedure_new(e, "<=", 2, -1, escm_le);
    (void) escm_procedure_new(e, ">=", 2, -1, escm_ge);
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
escm_number_p(escm *e, escm_atom *args)
{
    return (ESCM_ISNUMBER(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_integer_p(escm *e, escm_atom *args)
{
    escm_atom *arg;
    escm_number *n;

    arg = escm_cons_pop(e, &args);
    if (!ESCM_ISNUMBER(arg))
       return e->FALSE;
    n = arg->ptr;
    if (n->fixnum == 1)
       return e->TRUE;
#ifdef ESCM_USE_MATH
    return (DBL_EQ(n->d.rval, round(n->d.rval))) ? e->TRUE : e->FALSE;
#else
    return e->FALSE;
#endif
}

escm_atom *
escm_real_p(escm *e, escm_atom *args)
{
    return (ESCM_ISNUMBER(escm_cons_val(args)->car)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_zero_p(escm *e, escm_atom *args)
{
    escm_atom *arg;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(arg), arg, e);
    if (ESCM_NUMBER_ISINT(arg))
       return (escm_number_ival(arg) == 0) ? e->TRUE : e->FALSE;
    return (escm_number_rval(arg) == 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_positive_p(escm *e, escm_atom *args)
{
    escm_atom *arg;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(arg), arg, e);
    if (ESCM_NUMBER_ISINT(arg))
       return (escm_number_ival(arg) > 0) ? e->TRUE : e->FALSE;
    return (DBL_GT(escm_number_rval(arg), 0.)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_negative_p(escm *e, escm_atom *args)
{
    escm_atom *arg;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_ISNUMBER(arg), arg, e);
    if (ESCM_NUMBER_ISINT(arg))
       return (escm_number_ival(arg) < 0) ? e->TRUE : e->FALSE;
    return (DBL_LT(escm_number_rval(arg), 0.)) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_odd_p(escm *e, escm_atom *args)
{
    escm_atom *arg;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_NUMBER_ISINT(arg), arg, e);

    return (escm_number_ival(arg) % 2 == 1) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_even_p(escm *e, escm_atom *args)
{
    escm_atom *arg;

    arg = escm_cons_pop(e, &args);
    escm_assert(ESCM_NUMBER_ISINT(arg), arg, e);

    return (escm_number_ival(arg) % 2 == 0) ? e->TRUE : e->FALSE;
}

escm_atom *
escm_add(escm *e, escm_atom *params)
{
    escm_number *a, *b;
    escm_atom *c;

    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 0;

    c = escm_cons_pop(e, &params);
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

	c = escm_cons_pop(e, &params);
    }

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_sub(escm *e, escm_atom *params)
{
    escm_number *a, *b;
    escm_atom *c;

    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 0;

    c = escm_cons_pop(e, &params);

    if (params) {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));

	memcpy(a, c->ptr, sizeof *a);

	c = escm_cons_pop(e, &params);
    }

    do {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
	b = ((escm_number *) c->ptr);

	if (a->fixnum) {
	    if (b->fixnum)
		a->d.ival -= b->d.ival;
	    else {
		long tmp;

		tmp = a->d.ival;
		a->d.rval = (double) tmp;
		a->d.rval -= b->d.rval;
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
escm_mul(escm *e, escm_atom *params)
{
    escm_number *a, *b;
    escm_atom *c;

    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 1;

    c = escm_cons_pop(e, &params);
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

	c = escm_cons_pop(e, &params);
    }

    return escm_atom_new(e, ESCM_TYPE_NUMBER, a);
}

escm_atom *
escm_div(escm *e, escm_atom *params)
{
    escm_number *a, *b;
    escm_atom *c;

    a = xmalloc(sizeof *a);
    a->fixnum = 1, a->d.ival = 1;

    c = escm_cons_pop(e, &params);

    if (params) {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));

	memcpy(a, c->ptr, sizeof *a);

	c = escm_cons_pop(e, &params);
    }

    do {
	escm_assert1(ESCM_ISNUMBER(c), c, e, free(a));
	b = ((escm_number *) c->ptr);

	if ((b->fixnum) ? b->d.ival == 0 : DBL_EQ(b->d.rval, 0)) {
	    fprintf(stderr, "division by zero.\n");
	    e->err = -1;
	    return NULL;
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
    }

    return e->TRUE;
}

static void
number_print(escm *e, escm_number *number, FILE *stream)
{
    (void) e;

    if (number->fixnum)
	fprintf(stream, "%ld", number->d.ival);
    else
	fprintf(stream, "%.1f", number->d.rval);
}

static int
number_equal(escm *e, escm_number *n1, escm_number *n2, unsigned int lvl)
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
    escm_number *n;
    escm_atom *atom;
    char *sym;
    char *ec;
    int c, radix;

    n = xmalloc(sizeof *n);

    radix = 10;

    c = escm_input_getc(e->input);
    if (c == '#') {
	c = escm_input_getc(e->input);
	switch (c) {
	case 'b': radix = 2; break;
	case 'o': radix = 8; break;
	case 'd': radix = 10; break;
	case 'x': radix = 16; break;
	default: /* should never happen */
	    escm_input_print(e->input, "unknown character #%c.", c);
	    break;
	}
    } else
	escm_input_ungetc(e->input, c);

    atom = NULL;
    sym = escm_input_getsymbol(e->input);
    if (strchr(sym, '.') != NULL) { /* real */
	n->fixnum = 0;

	n->d.rval = strtod(sym, &ec);
    } else {
	n->fixnum = 1;

	n->d.ival = strtol(sym, &ec, radix);
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
    if (*ec == '\0')
	atom = escm_atom_new(e, numbertype, n);
    else {
	if (e->input->type == INPUT_FILE)
	    e->input->d.file.car -= strlen(sym) - (ec - sym) - 1;
	else
	    e->input->d.str.cur = (char *) e->input->d.str.str + (ec - sym + 1);
	escm_input_print(e->input, "Character `%c' unexpected.", *ec);
	e->err = -1;
	free(n);
    }

    free(sym);
    return atom;
}
