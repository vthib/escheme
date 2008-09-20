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
#ifndef ESCHEME_ASTRINGS_H
# define ESCHEME_ASTRINGS_H

#include "types.h"

#define ESCM_TYPE_ASTRING escm_astring_tget()

#define ESCM_ISASTR(x) ((x)->type == ESCM_TYPE_ASTRING)

#define escm_astr_val(x) (((escm_astring *) (x)->ptr)->str)
#define escm_astr_len(x) (((escm_astring *) (x)->ptr)->len)

typedef struct escm_astring {
    char *str;
    size_t len;
} escm_astring;

void escm_astrings_init(escm *);
size_t escm_astring_tget(void);
escm_atom *escm_astring_make(escm *, const char *, size_t);

escm_atom *escm_astring_p(escm *, escm_atom *, void *);
escm_atom *escm_make_astring(escm *, escm_atom *, void *);
escm_atom *escm_prim_astring(escm *, escm_atom *, void *);
escm_atom *escm_astring_length(escm *, escm_atom *, void *);
escm_atom *escm_astring_ref(escm *, escm_atom *, void *);
escm_atom *escm_astring_set_x(escm *, escm_atom *, void *);

escm_atom *escm_astring_eq_p(escm *, escm_atom *, void *);
escm_atom *escm_astring_lt_p(escm *, escm_atom *, void *);
escm_atom *escm_astring_gt_p(escm *, escm_atom *, void *);
escm_atom *escm_astring_le_p(escm *, escm_atom *, void *);
escm_atom *escm_astring_ge_p(escm *, escm_atom *, void *);

escm_atom *escm_astring_ci_eq_p(escm *, escm_atom *, void *);
escm_atom *escm_astring_ci_lt_p(escm *, escm_atom *, void *);
escm_atom *escm_astring_ci_gt_p(escm *, escm_atom *, void *);
escm_atom *escm_astring_ci_le_p(escm *, escm_atom *, void *);
escm_atom *escm_astring_ci_ge_p(escm *, escm_atom *, void *);

escm_atom *escm_subastring(escm *, escm_atom *, void *);
escm_atom *escm_astring_append(escm *, escm_atom *, void *);
escm_atom *escm_astring_copy(escm *, escm_atom *, void *);
escm_atom *escm_astring_fill_x(escm *, escm_atom *, void *);

escm_atom *escm_astring_to_list(escm *, escm_atom *, void *);
escm_atom *escm_list_to_astring(escm *, escm_atom *, void *);

#endif /* ESCHEME_ASTRINGS_H */
