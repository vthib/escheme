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

#define ESCM_TYPE_CONS (escm_cons_tget())

#define ESCM_ISCONS(x) ((x)->type == ESCM_TYPE_CONS)

#define escm_cons_next(x) (ESCM_ISCONS((x)->cdr) ? \
                           escm_cons_val((x)->cdr) : NULL)

#define escm_cons_val(x) ((escm_cons *) (x)->ptr)
#define escm_cons_car(x) (escm_cons_val(x)->car)
#define escm_cons_cdr(x) (escm_cons_val(x)->cdr)

typedef struct escm_cons {
    escm_atom *car;
    escm_atom *cdr;
} escm_cons;

void escm_cons_init(escm *);
size_t escm_cons_tget(void);

escm_atom *escm_cons_make(escm *, escm_atom *, escm_atom *);

escm_atom *escm_cons_pop(escm *, escm_atom **);
int escm_cons_isin(escm *, escm_atom *, escm_atom *, int);

#endif /* ESCHEME_CONS_H */
