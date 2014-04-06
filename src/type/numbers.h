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
#ifndef ESCHEME_NUMBERS_H
# define ESCHEME_NUMBERS_H

#ifdef ESCM_USE_NUMBERS

# include "types.h"

# define ESCM_TYPE_NUMBER escm_number_tget()

# define ESCM_ISNUMBER(x) ((x)->type == ESCM_TYPE_NUMBER)
# define ESCM_ISINT(x) (ESCM_ISNUMBER(x) &&                      \
                        ((escm_number *) (x)->ptr)->fixnum == 1)
# define ESCM_ISREAL(x) (!ESCM_ISBINT(x))

# define escm_number_ival(x) (((escm_number *) (x)->ptr)->d.ival)
# define escm_number_rval(x) (((escm_number *) (x)->ptr)->d.rval)

typedef struct escm_number {
    union {
        long ival;
        double rval;
    } d;

    unsigned int fixnum : 1;
} escm_number;

void escm_numbers_init(escm *);
size_t escm_number_tget(void);

escm_atom *escm_int_make(escm *, long);
escm_atom *escm_real_make(escm *, double);

escm_atom *escm_number_parse(escm *e, escm_input *stream, int base);

escm_atom *escm_length(escm *, escm_atom *, void *);
escm_atom *escm_list_tail(escm *, escm_atom *, void *);
escm_atom *escm_list_ref(escm *, escm_atom *, void *);

escm_atom *escm_number_p(escm *, escm_atom *, void *);
escm_atom *escm_integer_p(escm *, escm_atom *, void *);
escm_atom *escm_real_p(escm *, escm_atom *, void *);

escm_atom *escm_eq(escm *, escm_atom *, void *);
escm_atom *escm_lt(escm *, escm_atom *, void *);
escm_atom *escm_gt(escm *, escm_atom *, void *);
escm_atom *escm_le(escm *, escm_atom *, void *);
escm_atom *escm_ge(escm *, escm_atom *, void *);

escm_atom *escm_add(escm *, escm_atom *, void *);
escm_atom *escm_sub(escm *, escm_atom *, void *);
escm_atom *escm_mul(escm *, escm_atom *, void *);
escm_atom *escm_div(escm *, escm_atom *, void *);

escm_atom *escm_quotient(escm *, escm_atom *, void *);
escm_atom *escm_remainder(escm *, escm_atom *, void *);
escm_atom *escm_modulo(escm *, escm_atom *, void *);

escm_atom *escm_gcd(escm *, escm_atom *, void *);
escm_atom *escm_lcm(escm *, escm_atom *, void *);

escm_atom *escm_numerator(escm *, escm_atom *, void *);
escm_atom *escm_denominator(escm *, escm_atom *, void *);

escm_atom *escm_rand(escm *, escm_atom *, void *);

escm_atom *escm_floor(escm *, escm_atom *, void *);
escm_atom *escm_ceiling(escm *, escm_atom *, void *);
escm_atom *escm_truncate(escm *, escm_atom *, void *);
escm_atom *escm_round(escm *, escm_atom *, void *);

escm_atom *escm_exp(escm *, escm_atom *, void *);
escm_atom *escm_log(escm *, escm_atom *, void *);
escm_atom *escm_sin(escm *, escm_atom *, void *);
escm_atom *escm_cos(escm *, escm_atom *, void *);
escm_atom *escm_tan(escm *, escm_atom *, void *);
escm_atom *escm_asin(escm *, escm_atom *, void *);
escm_atom *escm_acos(escm *, escm_atom *, void *);
escm_atom *escm_atan(escm *, escm_atom *, void *);

escm_atom *escm_sqrt(escm *, escm_atom *, void *);
escm_atom *escm_expt(escm *, escm_atom *, void *);

#endif /* ESCM_USE_NUMBERS */

#endif /* ESCHEME_NUMBERS_H */
