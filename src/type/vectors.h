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
#ifndef ESCHEME_VECTORS_H
# define ESCHEME_VECTORS_H

#ifdef ESCM_USE_VECTORS

# include "types.h"

# define ESCM_TYPE_VECTOR escm_vector_tget()

# define ESCM_ISVECTOR(x) ((x)->type == ESCM_TYPE_VECTOR)

# define escm_vector_val(x) ((escm_vector *) (x)->ptr)
# define escm_vector_len(x) (escm_vector_val(x)->len)

typedef struct escm_vector {
    escm_atom **vec;
    size_t len;
} escm_vector;

void escm_vectors_init(escm *);
size_t escm_vector_tget(void);

escm_atom *escm_vector_make(escm *, escm_atom **, size_t);

escm_atom *escm_vector_p(escm *, escm_atom *, void *);

escm_atom *escm_prim_vector(escm *, escm_atom *, void *);
escm_atom *escm_make_vector(escm *, escm_atom *, void *);
escm_atom *escm_vector_length(escm *, escm_atom *, void *);
escm_atom *escm_vector_ref(escm *, escm_atom *, void *);
escm_atom *escm_vector_set_x(escm *, escm_atom *, void *);
escm_atom *escm_vector_fill_x(escm *, escm_atom *, void *);

escm_atom *escm_vector_to_list(escm *, escm_atom *, void *);
escm_atom *escm_list_to_vector(escm *, escm_atom *, void *);

#endif /* ESCM_USE_VECTORS */

#endif /* ESCHEME_VECTORS_H */
