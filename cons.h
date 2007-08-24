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
#ifndef ESCHEME_CONS_H
# define ESCHEME_CONS_H

#include "types.h"

#define ESCM_TYPE_CONS 1 /* XXX: ugly, hardcoded and not relevant if we change
			    the order of the initialisation */

#define ESCM_CONS_NEXT(x) (ESCM_ISCONS((x)->cdr)) ? \
    ESCM_CONS_VAL((x)->cdr) : NULL

#define ESCM_ISCONS(x) ((x)->type == ESCM_TYPE_CONS)

#define ESCM_CONS_VAL(x) ((escm_cons *) (x)->ptr)

typedef struct escm_cons {
    escm_atom *car;
    escm_atom *cdr;
} escm_cons;

void escm_cons_init(escm *);

escm_atom *escm_cons_new(escm *, escm_atom *, escm_atom *);

void escm_cons_put(escm *, escm_atom **, escm_atom *);
escm_atom *escm_cons_pop(escm *, escm_atom **);

escm_atom *escm_prim_cons(escm *, escm_atom *);
escm_atom *escm_list(escm *, escm_atom *);

escm_atom *escm_car(escm *, escm_atom *);
escm_atom *escm_cdr(escm *, escm_atom *);
escm_atom *escm_set_car_x(escm *, escm_atom *);
escm_atom *escm_set_cdr_x(escm *, escm_atom *);

escm_atom *escm_null_p(escm *, escm_atom *);
escm_atom *escm_pair_p(escm *, escm_atom *);
escm_atom *escm_list_p(escm *, escm_atom *);

escm_atom *escm_length(escm *, escm_atom *);
escm_atom *escm_append(escm *, escm_atom *);
escm_atom *escm_reverse(escm *, escm_atom *);

escm_atom *escm_list_tail(escm *, escm_atom *);
escm_atom *escm_list_ref(escm *, escm_atom *);

#endif /* ESCHEME_CONS_H */
