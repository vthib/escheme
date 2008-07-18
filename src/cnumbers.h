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
#ifndef ESCHEME_CNUMBERS_H
# define ESCHEME_CNUMBERS_H

#include "types.h"

#define ESCM_TYPE_CNUMBER escm_cnumber_tget()

#define ESCM_ISCNUMBER(x) ((x)->type == ESCM_TYPE_CNUMBER)
#define ESCM_ISCINT(x) (ESCM_ISNUMBER(x) &&				\
		       ((escm_number *) (x)->ptr)->type == ESCM_INTEGER)
#define ESCM_ISCREAL(x) (ESCM_ISNUMBER(x) &&				\
			((escm_number *) (x)->ptr)->type == ESCM_REAL)
#define ESCM_ISRATIONAL(x) (ESCM_ISNUMBER(x) &&				\
			    ((escm_number *) (x)->ptr)->type == ESCM_RATIONAL)
#define ESCM_ISCOMPLEX(x) (ESCM_ISNUMBER(x) &&				\
			   ((escm_number *) (x)->ptr)->type == ESCM_COMPLEX)

#define escm_cnumber_val(x) ((escm_number *) (x)->ptr)
#define escm_cnumber_ival(x) (escm_cnumber_val(x)->d.i)
#define escm_cnumber_rval(x) (escm_cnumber_val(x)->d.real)

#define ESCM_ISCTRUE(x) (!x || !ESCM_ISINT(x) || \
			 (((escm_number *) (x)->ptr)->d.i != 0))

#define escm_cnumber_exactp(x) (escm_cnumber_val(x)->exact)

typedef struct escm_number escm_number;

typedef struct escm_rational {
    long n;
    long d;
} escm_rational;

typedef struct escm_complex {
    escm_number *im;
    escm_number *re;
} escm_complex;

typedef enum Escm_Number_Type {
    ESCM_INTEGER,
    ESCM_REAL,
    ESCM_RATIONAL,
    ESCM_COMPLEX
} Escm_Number_Type;

struct escm_number {
    union {
	long i;
	double real;
	escm_rational rat;
	escm_complex cpx;
    } d;

    Escm_Number_Type type;

    unsigned int exact : 1;
};

void escm_cnumbers_init(escm *);
size_t escm_cnumber_tget(void);

escm_atom *escm_cint_make(escm *, long, int);
escm_atom *escm_creal_make(escm *, double);

void escm_number_add(escm *, escm_number **, escm_number *);
void escm_number_sub(escm *, escm_number **, escm_number *);
void escm_number_mul(escm *, escm_number **, escm_number *);
void escm_number_div(escm *, escm_number **, escm_number *);

escm_atom *escm_number_p(escm *, escm_atom *, void *);

escm_atom *escm_exact_p(escm *, escm_atom *);
escm_atom *escm_inexact_p(escm *, escm_atom *);

escm_atom *escm_eq(escm *, escm_atom *);
escm_atom *escm_lt(escm *, escm_atom *);
escm_atom *escm_gt(escm *, escm_atom *);
escm_atom *escm_le(escm *, escm_atom *);
escm_atom *escm_ge(escm *, escm_atom *);

escm_atom *escm_add(escm *, escm_atom *);
escm_atom *escm_sub(escm *, escm_atom *);
escm_atom *escm_mul(escm *, escm_atom *);
escm_atom *escm_div(escm *, escm_atom *);

escm_atom *escm_quotient(escm *, escm_atom *);
escm_atom *escm_remainder(escm *, escm_atom *);
escm_atom *escm_modulo(escm *, escm_atom *);

escm_atom *escm_gcd(escm *, escm_atom *);
escm_atom *escm_lcm(escm *, escm_atom *);

escm_atom *escm_numerator(escm *, escm_atom *);
escm_atom *escm_denominator(escm *, escm_atom *);

#ifdef ESCM_USE_MATH
escm_atom *escm_floor(escm *, escm_atom *);
escm_atom *escm_ceiling(escm *, escm_atom *);
escm_atom *escm_truncate(escm *, escm_atom *);
escm_atom *escm_round(escm *, escm_atom *);

escm_atom *escm_exp(escm *, escm_atom *);
escm_atom *escm_log(escm *, escm_atom *);
escm_atom *escm_sin(escm *, escm_atom *);
escm_atom *escm_cos(escm *, escm_atom *);
escm_atom *escm_tan(escm *, escm_atom *);
escm_atom *escm_asin(escm *, escm_atom *);
escm_atom *escm_acos(escm *, escm_atom *);
escm_atom *escm_atan(escm *, escm_atom *);

escm_atom *escm_sqrt(escm *, escm_atom *);
escm_atom *escm_expt(escm *, escm_atom *);
#endif

escm_atom *escm_exact_to_inexact(escm *, escm_atom *);
escm_atom *escm_inexact_to_exact(escm *, escm_atom *);

#ifdef ESCM_USE_STRINGS
escm_atom *escm_number_to_string(escm *, escm_atom *);
escm_atom *escm_string_to_number(escm *, escm_atom *);
#endif

#endif /* ESCHEME_CNUMBERS_H */
