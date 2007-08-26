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

#include "types.h"

#define ESCM_TYPE_NUMBER escm_number_tget()

#define ESCM_ISNUMBER(x) ((x)->type == ESCM_TYPE_NUMBER)

#define ESCM_NUMBER_ISINT(x) (ESCM_ISNUMBER(x) && \
			      ((escm_number *) (x)->ptr)->fixnum == 1)
#define ESCM_NUMBER_IVAL(x) (((escm_number *) (x)->ptr)->d.ival)
#define ESCM_NUMBER_RVAL(x) (((escm_number *) (x)->ptr)->d.rval)

#ifdef ESCM_INTBOOL
# define ESCM_ISTRUE(x) (!x || !ESCM_ISNUMBER(x) || \
			 (((escm_number *) (x)->ptr)->fixnum == 1) ? \
			 ESCM_NUMBER_IVAL(x) != 0 : \
			 DBL_EQ(0., ESCM_NUMBER_RVAL(x)))
#endif

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

escm_atom *escm_add(escm *, escm_atom *);
escm_atom *escm_sub(escm *, escm_atom *);
escm_atom *escm_mul(escm *, escm_atom *);
escm_atom *escm_div(escm *, escm_atom *);

escm_atom *escm_eq(escm *, escm_atom *);
escm_atom *escm_lt(escm *, escm_atom *);
escm_atom *escm_gt(escm *, escm_atom *);
escm_atom *escm_le(escm *, escm_atom *);
escm_atom *escm_ge(escm *, escm_atom *);

#endif /* ESCHEME_NUMBERS_H */
