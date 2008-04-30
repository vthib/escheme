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
#ifndef ESCHEME_ATOM_H
# define ESCHEME_ATOM_H

#include "types.h"

struct escm_atom {
    size_t type;

    void *ptr;

    escm_atom *link;
    escm_atom *env;

    unsigned int marked : 1;
    unsigned int nofree : 1;
    unsigned int ro : 1;
};

escm_atom *escm_atom_new(escm *, size_t, void *);
void escm_atom_free(escm *, escm_atom *);

void escm_atom_mark(escm *, escm_atom *);
escm_atom *escm_atom_eval(escm *, escm_atom *);
escm_atom *escm_atom_eval3(escm *, escm_atom *, int);
escm_atom *escm_atom_exec(escm *, escm_atom *, escm_atom *);

#define escm_atom_print(e, atom) \
    escm_atom_print4(e, atom, e->output, 0)
#define escm_atom_write(e, atom) escm_atom_print(e, atom)

#define escm_atom_display(e, atom) \
    escm_atom_print4(e, atom, e->output, 1)
#define escm_atom_printerr(e, atom) \
    escm_atom_print4(e, atom, e->errp, 1)

#define escm_atom_print3(e, atom, stream) \
    escm_atom_print4(e, atom, stream, 0)

/* 0: write, 1: display, others: implementation dependent */
void escm_atom_print4(escm *, escm_atom *, escm_output *, int);

/* 0: eq, 1: eqv, 2: equal, others: implementation dependent */
int escm_atom_equal(escm *, escm_atom *, escm_atom *, int);

#endif /* ESCHEME_ATOM_H */
