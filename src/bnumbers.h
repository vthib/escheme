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
#ifndef ESCHEME_BNUMBERS_H
# define ESCHEME_BNUMBERS_H

#include "types.h"

#define ESCM_TYPE_BNUMBER escm_bnumber_tget()

#define ESCM_ISBNUMBER(x) ((x)->type == ESCM_TYPE_BNUMBER)
#define ESCM_ISBINT(x) (ESCM_ISBNUMBER(x) && \
		       ((escm_bnumber *) (x)->ptr)->fixnum == 1)
#define ESCM_ISBREAL(x) (!ESCM_ISBINT(x))

#define escm_bnumber_ival(x) (((escm_bnumber *) (x)->ptr)->d.ival)
#define escm_bnumber_rval(x) (((escm_bnumber *) (x)->ptr)->d.rval)

#define ESCM_ISBTRUE(x) (!ESCM_ISBINT(x) || escm_bnumber_ival(x) != 0)

typedef struct escm_bnumber {
    union {
	long ival;
	double rval;
    } d;

    unsigned int fixnum : 1;
} escm_bnumber;

void escm_bnumbers_init(escm *);
size_t escm_bnumber_tget(void);

escm_atom *escm_bint_make(escm *, long);
escm_atom *escm_breal_make(escm *, double);

escm_atom *escm_bnumber_p(escm *, escm_atom *);
escm_atom *escm_binteger_p(escm *, escm_atom *);
escm_atom *escm_breal_p(escm *, escm_atom *);

escm_atom *escm_beq(escm *, escm_atom *);
escm_atom *escm_blt(escm *, escm_atom *);
escm_atom *escm_bgt(escm *, escm_atom *);
escm_atom *escm_ble(escm *, escm_atom *);
escm_atom *escm_bge(escm *, escm_atom *);

escm_atom *escm_badd(escm *, escm_atom *);
escm_atom *escm_bsub(escm *, escm_atom *);
escm_atom *escm_bmul(escm *, escm_atom *);
escm_atom *escm_bdiv(escm *, escm_atom *);

escm_atom *escm_bquotient(escm *, escm_atom *);
escm_atom *escm_bremainder(escm *, escm_atom *);
escm_atom *escm_bmodulo(escm *, escm_atom *);

escm_atom *escm_bgcd(escm *, escm_atom *);
escm_atom *escm_blcm(escm *, escm_atom *);

escm_atom *escm_bnumerator(escm *, escm_atom *);
escm_atom *escm_bdenominator(escm *, escm_atom *);

#ifdef ESCM_USE_MATH
escm_atom *escm_bfloor(escm *, escm_atom *);
escm_atom *escm_bceiling(escm *, escm_atom *);
escm_atom *escm_btruncate(escm *, escm_atom *);
escm_atom *escm_bround(escm *, escm_atom *);

escm_atom *escm_bexp(escm *, escm_atom *);
escm_atom *escm_blog(escm *, escm_atom *);
escm_atom *escm_bsin(escm *, escm_atom *);
escm_atom *escm_bcos(escm *, escm_atom *);
escm_atom *escm_btan(escm *, escm_atom *);
escm_atom *escm_basin(escm *, escm_atom *);
escm_atom *escm_bacos(escm *, escm_atom *);
escm_atom *escm_batan(escm *, escm_atom *);

escm_atom *escm_bsqrt(escm *, escm_atom *);
escm_atom *escm_bexpt(escm *, escm_atom *);
#endif

#ifdef ESCM_USE_STRINGS
escm_atom *escm_bnumber_to_string(escm *, escm_atom *);
escm_atom *escm_string_to_bnumber(escm *, escm_atom *);
#endif

#endif /* ESCHEME_BNUMBERS_H */
